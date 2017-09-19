// W-file
// a : 00001010101;
// b : 11010100101;

// F-file
// x <= a or b;
// y <= a and b and '0';

;; store a
;; store b

li $1, 0
loop_x:
	load $2, a, 0
	load $3, b, 0
	or $4, $2, $3
	store $4, x, 0

	add a, a, 1
	add b, b, 1
	add x, x, 1

	add $1, $1, 1
	bne $1, 11, loop_x ;; length of W
