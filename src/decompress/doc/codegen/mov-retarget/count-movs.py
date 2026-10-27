import sys,re
L=[]
for l in open(sys.argv[1]):
    if l.startswith('; Size') : continue
    m=re.match(r'^;?\s*(?:[0-9A-F]+:\s+)?(?:L\d+:\s+)?[0-9A-F]+\s+(.*)$',l.rstrip())
    if m and not l.startswith(';   ') : L.append(m.group(1).split(';')[0].strip())
mov=re.compile(r'^MOV ([RE]\w+), ([RE]\w+)$')
print('insns',len(L),'reg-reg MOV',sum(1 for l in L if mov.match(l)),'stack',sum(1 for l in L if 'RBP-' in l),'SAR/SHL1',sum(1 for l in L if re.match(r'^(SAR|SHL) \w+, 1$',l)))
