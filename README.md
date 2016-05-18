# tsplisp

Nathan Gibson
January 11, 2015

 Program solves the traveling salesman problem by taking a list of cities
 and coordinates, permuting all possible orderings of the list and then
 choosing the ordering that requires the least distance to be traveled. 

 example usage: (printcities (travel '((Atlanta (50 . 50))
                                       (Orlando (100 . -225))
                                       (Knoxville (60 . 100))
                                       (Dothan (10 . -75)))
                              )
                )
