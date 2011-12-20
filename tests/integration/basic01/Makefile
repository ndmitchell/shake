default: test

include ../testconfig.mk

.PHONY: test default clean

test:
	echo "AAA" > A.txt
	echo "BBB" > B.txt
	$(RUNHC) shakefile.hs
	echo "AAA\nBBB" > AB.txt.expected
	diff -u AB.txt AB.txt.expected
	sleep 1
	echo "aaa" >> A.txt
	$(RUNHC) shakefile.hs
	echo "AAA\naaa\nBBB" > AB.txt.expected
	diff -u AB.txt AB.txt.expected

clean:
	rm -f A.txt B.txt AB.txt AB.txt.expected \
		.shake.database .shake.journal