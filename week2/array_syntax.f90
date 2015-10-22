!!!!!
!  This simple example shows the power of array syntax in Fortran.
!
!!!!!
program array_syntax
   implicit none
   integer :: A(5), B(5), C(5)

!! initialization of arrays
!
   A = 1
   B = 2

!! computation using arrays
!
   C = A + 13*B

!! dump out the results (note these really are arrays)
!
   print *, "C: ", C

end program array_syntax
