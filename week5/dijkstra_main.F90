program dijkstra_main
  use dijkstra
  use frontier_list
  use MPI_f08
  implicit none
  double precision :: time, time_relax = 0.0d0, time_min = 0.0d0
  integer :: i, j, k, ll = 0
  logical :: done  = .FALSE.
  logical :: debug = .FALSE.

  ! test list stuff

  ! type(Node) :: aNode
  ! integer(C_INT) :: len
  
  ! call Push(frontier, 1.0d0, 1, 0, 0)
  ! call Push(frontier, 2.0d0, 2, 0, 0)
  ! call Push(frontier, 3.0d0, 3, 0, 0)

  ! call Head(frontier, aNode)
  ! print *, aNode%tt, aNode%i, aNode%j, aNode%k

  ! call updateNodeTT(frontier, 33.0d0, 3, 0, 0)
  ! call Head(frontier, aNode)
  ! print *, aNode%tt, aNode%i, aNode%j, aNode%k

  ! print *, "length =", getLength(frontier)

  ! call deleteMinNode(frontier, time, i, j, k, len)
  ! print *, "min time =", real(time), i, j, k, len

  ! call deleteMinNode(frontier, time, i, j, k, len)
  ! print *, "min time =", real(time), i, j, k, len

  ! call deleteMinNode(frontier, time, i, j, k, len)
  ! print *, "min time =", real(time), i, j, k, len

  ! print *, "length =", getLength(frontier)

  ! stop

!---------------------------------------


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
     call relax_with_frontier(i,j,k)
     time_relax = time_relax + MPI_Wtime() - time

     !! find unfinished node with minimum travel time
     !
     time = MPI_Wtime()
     done = min_tt_frontier(i,j,k)
     Finished(i,j,k) = .TRUE.

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
