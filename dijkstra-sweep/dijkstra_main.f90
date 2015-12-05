program dijkstra_main
  use dijkstra
  use MPI_f08
  implicit none
  double precision :: time, time_relax = 0.0d0
  integer :: i, j, k, ll = 0
  logical :: changed
  logical :: done  = .FALSE.
  logical :: debug = .FALSE.
  
  call initialize()

  !! relax grid starting at (1,1,1)
  !
  i = 1;  j = 1;  k = 1;
  V(i,j,k)%tt = 0.0

  do while (.NOT. done) 
     if (debug) then
        print *, "NEXT:", i, j, k, V(i,j,k)%tt
     end if

     !! sweep over all points
     !
     changed = .FALSE.
     time = MPI_Wtime()
     do k = 1, NZ
        do j = 1, NY
           do i = 1, NX
              if ( relax(i,j,k) ) changed = .TRUE.
           end do
        end do
     end do
     time_relax = time_relax + MPI_Wtime() - time

     if (.NOT. changed) done = .TRUE.

     ll = ll + 1
  end do

  if (debug) then
     print *
     do i = 1, NX
        do j = 1, NY
           do k = 1, NZ
              print *, i, j, k, V(i,j,k)%tt 
           end do
        end do
     end do
  end if

  print *
  print *, "Relaxation time for N=", ll, NX*NY*NZ, real(time_relax)

  call finalize

end program
