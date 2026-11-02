;;;; drawable.lisp — XID wrapper classes, visual tracking, format lookup, and finalizers
;;;; Copyright (C) 2012–2014 Michael Filonenko and contributors
;;;;   (clx-truetype, from which this library is ported; see AUTHORS)
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:runes/xcb)

(defun %finalize (object fn)
  #+sbcl (sb-ext:finalize object fn :dont-save t)
  #-sbcl (trivial-garbage:finalize object fn))

;;; Deferred frees.
;;;
;;; Finalizers run on the implementation's finalizer thread, but CLXCB's
;;; SEND-REQUEST is not synchronised: allocating the serial and writing the
;;; request are two separate steps.  A finalizer that wrote FreePixmap
;;; directly could interleave with a request the application thread is
;;; sending, corrupting the byte stream or the client's sequence count.
;;; Finalizers therefore only queue (KIND . ID) entries, per connection;
;;; FLUSH-DEFERRED-FREES sends them from the application's thread.
;;;
;;; The queue is weak on the connection, like *FILL-CACHE* and
;;; *MASK-GC-CACHE*: a connection that is dropped without being closed
;;; still has an open stream, so it cannot be recognised as dead by
;;; testing it, but once it is unreachable its bucket goes with it.  The
;;; lock is recursive in case an implementation runs a finalizer on the
;;; thread that already holds it.

(defvar *deferred-frees*
  (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq)
  "Maps a CONNECTION to a list of (KIND . ID) frees queued by its
   drawables' finalizers.  Weak on the connection key.  Guarded by
   *DEFERRED-FREES-LOCK*.")

(defvar *deferred-frees-lock*
  (bt:make-recursive-lock "xcb-truetype deferred frees"))

(defun connection-open-p (connection)
  (let ((stream (xcb:conn-stream connection)))
    (and stream (open-stream-p stream))))

;;  Queue a free of ID on CONNECTION.  Safe to call from a finalizer: it
;;  touches only the queue, never the connection's socket.
(defun %defer-free (connection kind id)
  (bt:with-recursive-lock-held (*deferred-frees-lock*)
    (push (cons kind id) (gethash connection *deferred-frees*))))

;;  Remove and return CONNECTION's queued (KIND . ID) entries, oldest first.
(defun %take-deferred-frees (connection)
  (bt:with-recursive-lock-held (*deferred-frees-lock*)
    (let ((entries (gethash connection *deferred-frees*)))
      (when entries
        (remhash connection *deferred-frees*)
        (reverse entries)))))

(defun flush-deferred-frees (connection)
  "Send the free requests queued by finalizers of drawables that belonged
   to CONNECTION.  Must be called from the thread that owns CONNECTION.
   DRAW-TEXT, DRAW-TEXT-LINE and DESTROY-DRAWABLE call it automatically.
   The metric functions never touch a connection (they take a screen or a
   DPI), so a program that only measures text has no hook to piggyback
   on and should call this itself if it abandons drawables."
  (let ((entries (%take-deferred-frees connection)))
    ;; A closed connection's resources were freed by the server on
    ;; disconnect; there is nothing to send.
    (when (connection-open-p connection)
      (loop for (kind . id) in entries
            do (ignore-errors
                 (ecase kind
                   (:picture (free-picture connection id))
                   (:pixmap  (xcb:free-pixmap connection id))
                   (:window  (xcb:destroy-window connection id))))))))

;;; Drawables, and who frees what
;;;
;;; In the X protocol a DRAWABLE is only a type of XID: one that names a
;;; window or a pixmap.  The resources themselves, windows, pixmaps, and
;;; pictures, are what the server keeps, and whoever creates a resource
;;; is responsible for freeing it.  This library's DRAWABLE is a Lisp
;;; object around such an XID: it carries the connection the XID belongs
;;; to, the screen and visual, and the RENDER picture the library makes for
;;; drawing, which the library created and so always frees.
;;;
;;; Two facts about a drawable, kept apart:
;;;
;;;   What it is: the class.  A WINDOW or a PIXMAP; it decides which
;;;   request frees it, DestroyWindow or FreePixmap.  DRAWABLE is only
;;;   their common base and is not made itself.
;;;
;;;   Whether it is ours: :OWNED.  NIL (the default): the window or
;;;   pixmap belongs to whoever made it, an application's window, a
;;;   pixmap it manages, and only the picture is freed.  T: it was handed
;;;   to the drawable, and DESTROY-DRAWABLE -- or the garbage collector,
;;;   when the drawable is dropped -- frees it too.
;;;
;;;   (make-instance 'xcb-truetype:window :connection c :id win)
;;;   (make-instance 'xcb-truetype:pixmap :connection c :id pm :owned t)

(defclass drawable ()
  ((connection :initarg :connection :reader drawable-connection
               :documentation "Underlying XCB connection object.")
   (id         :initarg :id         :reader drawable-id
               :documentation "XID of the window or pixmap.")
   (owned      :initarg :owned      :initform nil :reader drawable-owned-p
               :documentation "True when the window or pixmap was handed to the
               drawable, and DESTROY-DRAWABLE or the garbage collector frees it;
               NIL when it belongs to whoever made it.")
   (visual     :initarg :visual     :initform nil :accessor drawable-visual
               :documentation "Visual ID associated with this drawable.
               Defaults to the root visual if unspecified.")
   (screen     :initarg :screen     :initform nil :accessor drawable-screen-slot
               :documentation "Screen associated with this drawable.
               Defaults to the connection's default screen if unspecified.")
   (armed      :initform nil
               :documentation "True while the finalizer is registered: from
               creation until DESTROY-DRAWABLE or RELEASE-DRAWABLE, and again
               once a new picture is made on the drawable after that.")
   (picture-box :initform (list nil) :reader drawable-picture-box
               :documentation "A one-element list holding the destination RENDER
               picture for this drawable, created lazily by DRAWABLE-PICTURE-FOR
               and freed by DESTROY-DRAWABLE.  Boxed so the finalizer can see
               a picture created after the finalizer was registered."))
  (:documentation "The base of WINDOW and PIXMAP: an XID that text is drawn on,
   with the connection it belongs to, so no call site can pair the wrong two,
   and whether the drawable owns it (:OWNED).  Make a WINDOW or a PIXMAP."))

(defclass window (drawable) ()
  (:documentation "A window that text is drawn on.  With :OWNED T,
   DESTROY-DRAWABLE destroys it."))

(defclass pixmap (drawable) ()
  (:documentation "A pixmap that text is drawn on.  With :OWNED T,
   DESTROY-DRAWABLE frees it."))

(defgeneric %free-request (drawable)
  (:documentation "Send the request that frees DRAWABLE's X object.")
  (:method ((w window)) (xcb:destroy-window (drawable-connection w) (drawable-id w)))
  (:method ((p pixmap)) (xcb:free-pixmap (drawable-connection p) (drawable-id p))))

(defun drawable-picture (drawable)
  "Return DRAWABLE's RENDER picture ID, or NIL if none has been created yet."
  (car (drawable-picture-box drawable)))

(defun (setf drawable-picture) (picture drawable)
  (setf (car (drawable-picture-box drawable)) picture))

(defun default-screen (connection)
  "Return the primary root screen for CONNECTION."
  (first (xcb:conn-roots connection)))

(defgeneric drawable-screen (drawable)
  (:documentation "Return the XCB screen for DRAWABLE."))

(defmethod drawable-screen ((s xcb:screen))
  s)

(defmethod drawable-screen ((d drawable))
  (or (drawable-screen-slot d)
      (default-screen (drawable-connection d))))

(defun drawable-visual-id (d)
  "Return the visual ID for DRAWABLE, defaulting to its screen's root visual."
  (let ((vis (or (drawable-visual d)
                 (xcb:root-visual (drawable-screen d)))))
    ;; ROOT-VISUAL returns a CARD32 numeric ID; DRAWABLE-VISUAL may be either
    ;; a numeric ID or a visualtype object for future-proofing.
    (if (numberp vis)
        vis
        (xcb:visual-id vis))))

(defun screen-dpi (screen)
  "Return (VALUES DPI-X DPI-Y) for SCREEN."
  (let ((w-mm (xcb:width-in-millimeters screen))
        (h-mm (xcb:height-in-millimeters screen)))
    (values (if (plusp w-mm)
                (floor (* (xcb:width-in-pixels screen) 25.4) w-mm)
                96)
            (if (plusp h-mm)
                (floor (* (xcb:height-in-pixels screen) 25.4) h-mm)
                96))))

(defgeneric dpi-x (drawable)
  (:documentation "Return horizontal DPI for DRAWABLE."))

(defmethod dpi-x ((d drawable))
  (nth-value 0 (screen-dpi (drawable-screen d))))

(defgeneric dpi-y (drawable)
  (:documentation "Return vertical DPI for DRAWABLE."))

(defmethod dpi-y ((d drawable))
  (nth-value 1 (screen-dpi (drawable-screen d))))

(defun pict-formats-reply-for (connection)
  "Return CONNECTION's RENDER QueryPictFormats reply, querying the server
   the first time and caching it on the connection after that.  Every
   format lookup in this library goes through it; a program looking up
   formats of its own can share it instead of querying again."
  (or (xcb:conn-pict-formats-reply connection)
      (setf (xcb:conn-pict-formats-reply connection)
            (xcb-render:query-pict-formats-info connection))))

(defun a8-format (connection)
  "Return the A8 PictFormat ID on CONNECTION, cached in a slot on the
   connection object. Looked up once per connection.
   Signals an error if the server does not enumerate an A8 format.  The
   failure is not cached, but a retry costs only a local search: the
   QueryPictFormats reply itself is cached on the connection."
  (or (xcb:conn-a8-format connection)
      (let* ((reply (pict-formats-reply-for connection))
             (fmt   (xcb-render:find-pict-format reply
                                                 :type 1 :depth 8
                                                 :red-mask 0 :green-mask 0 :blue-mask 0
                                                 :alpha-mask #xff)))
        (unless fmt
          (error "X server QueryPictFormats does not enumerate a standard A8 format (vendor: ~S, connection: ~S)"
                 (xcb:conn-vendor connection) connection))
        (setf (xcb:conn-a8-format connection) fmt))))

(defun format-for-visual (connection visual-id)
  "Return the RENDER PictFormat for VISUAL-ID on CONNECTION.  The
   QueryPictFormats reply is cached on the connection, so this is a local
   search with no round trip; the result itself is not cached."
  (xcb-render:find-visual-pict-format
   (pict-formats-reply-for connection)
   visual-id))

(defgeneric destroy-drawable (drawable)
  (:documentation "Free DRAWABLE's RENDER picture, and the window or pixmap
   too if DRAWABLE owns it (:OWNED T).  Not owning it, it is the same as
   RELEASE-DRAWABLE.  Idempotent."))

(defun release-drawable (drawable)
  "Free DRAWABLE's RENDER picture and cancel its finalizer, and leave the
   window or pixmap alone, owned or not.  For a drawable that does not own
   its window or pixmap this is what DESTROY-DRAWABLE does too; the two
   differ only for an owned one, which RELEASE-DRAWABLE hands back to the
   caller to free: DRAWABLE no longer owns it afterwards.  Idempotent."
  (flush-deferred-frees (drawable-connection drawable))
  (unwind-protect
       (when (drawable-picture drawable)
         (ignore-errors
           (free-picture (drawable-connection drawable) (drawable-picture drawable)))
         (setf (drawable-picture drawable) nil))
    ;; Handed back: a later DESTROY-DRAWABLE must not free a window or
    ;; pixmap the caller may have freed, its XID perhaps reused since.
    (setf (slot-value drawable 'owned) nil)
    (%disarm-finalizer drawable))
  drawable)

(defmethod initialize-instance :before ((d drawable) &key connection id &allow-other-keys)
  (unless (typep d '(or window pixmap))
    (error "Make a WINDOW or a PIXMAP (or a subclass of one): a ~S has no ~
            kind, so nothing would know how to free it."
           (class-name (class-of d))))
  (unless (and connection id)
    (error "A ~S requires both :connection and :id" (class-name (class-of d)))))

(defmethod destroy-drawable :around ((d drawable))
  ;; Cleanup is a natural point to drain frees queued by finalizers, and
  ;; covers programs that stop drawing but still destroy drawables.
  (flush-deferred-frees (drawable-connection d))
  (unwind-protect
       (progn
         (when (drawable-picture d)
           (ignore-errors
             (free-picture (drawable-connection d)
                           (drawable-picture d)))
           (setf (drawable-picture d) nil))
         (call-next-method))
    ;; The window or pixmap is gone now, if it was ours to free: a
    ;; drawable used again after this frees only its new picture.
    (setf (slot-value d 'owned) nil)
    (%disarm-finalizer d)))

(defmethod destroy-drawable ((d drawable))
  (when (drawable-owned-p d)
    (ignore-errors (%free-request d))))

(defmethod initialize-instance :after ((d drawable) &key)
  (%arm-finalizer d))

(defun %disarm-finalizer (d)
  (trivial-garbage:cancel-finalization d)
  (setf (slot-value d 'armed) nil))

(defun %arm-finalizer (d)
  "Register D's finalizer, unless it is registered."
  (unless (slot-value d 'armed)
    (%register-finalizer d)
    (setf (slot-value d 'armed) t)))

(defun %register-finalizer (d)
  ;; Close over ID, CONNECTION, KIND and the picture box, never over D
  ;; itself: a finalizer that closes over the object it finalizes would
  ;; keep that object alive forever, and the finalizer would never run.
  ;; The finalizer only queues the frees; see FLUSH-DEFERRED-FREES.
  ;;
  ;; The window or pixmap is freed with the drawable only when it is
  ;; owned: the garbage collector must not destroy an application's live
  ;; window behind its back.
  (let ((conn (drawable-connection d))
        (id   (drawable-id d))
        (box  (drawable-picture-box d))
        (kind (and (drawable-owned-p d)
                   (etypecase d (window :window) (pixmap :pixmap)))))
    (%finalize d
               (lambda ()
                 (when (connection-open-p conn)
                   (when (car box)
                     (%defer-free conn :picture (car box)))
                   (when kind
                     (%defer-free conn kind id)))))))

(defmethod target-dpi ((target xcb:screen))
  (screen-dpi target))

(defmethod target-dpi ((target drawable))
  (screen-dpi (drawable-screen target)))
