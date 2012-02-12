			 .oooooo..o                     oooo   o8o                      
			d8P'    `Y8                     `888   `"'                      
			Y88bo.       .ooooo.   .oooo.    888  oooo   .oooo.o oo.ooooo.  
			 `"Y8888o.  d88' `"Y8 `P  )88b   888  `888  d88(  "8  888' `88b 
			     `"Y88b 888        .oP"888   888   888  `"Y88b.   888   888 
			oo     .d8P 888   .o8 d8(  888   888   888  o.  )88b  888   888 
			8""88888P'  `Y8bod8P' `Y888""8o o888o o888o 8""888P'  888bod8P' 
			                                                      888       
			                                                     o888o      

A lisp interpreter in ~100 lines of Scala, inspired by [Peter Norvig](http://norvig.com/lispy.html)

It's very incomplete and probably buggy, but it works good enough to calculate
factorials and stuff: 

	(define fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))
	(fact 10) 

produces 3628800
