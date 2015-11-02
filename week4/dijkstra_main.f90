program dijkstra_main
  use dijkstra
  use MPI_f08
  implicit none
  double precision :: time, time_relax = 0.0d0, time_min = 0.0d0
  integer :: i, j, k, ll = 0
  logical :: done  = .FALSE.
  logical :: debug = .FALSE.
  
  call initialize

  !! relax grid starting at (1,1,1)
  !
  i = 1;  j = 1;  k = 1;
  V(i,j,k)%tt = 0.0

  do while (.NOT. done) 
     if (debug) then
        print *, "NEXT:", i, j, k, V(i,j,k)%tt
     end if

     time = MPI_Wtime()
     call relax_loops(i,j,k)
     time_relax = time_relax + MPI_Wtime() - time

     !! find unfinished node with minimum travel time
     !
     time = MPI_Wtime()
     done = min_tt_unfinished(i,j,k)
     time_min = time_min + MPI_Wtime() - time

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
  print *, "Relaxation/min time for N=", ll, NX*NY*NZ, real(time_relax), real(time_min)

  call finalize

end program
