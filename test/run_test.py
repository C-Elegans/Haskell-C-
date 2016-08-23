#!/bin/python
from subprocess import Popen, PIPE
import sys

if len(sys.argv) != 3:
    print "Incorrect args"

expected = open(sys.argv[2]).read().strip()


p = Popen(["d16-emu",sys.argv[1]], 
    stdin=PIPE, stdout=PIPE, stderr=PIPE)
output, err = p.communicate()
output = output.strip()
rc = p.returncode


if expected == output:
    sys.exit(0);
else:
    print "expected:"
    print expected
    print "actual"
    print output.strip()
    sys.exit(1);
    
