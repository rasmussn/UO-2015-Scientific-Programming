#define DEBUG 1

module dijkstra
  implicit none

  integer, parameter :: NX = 4
  integer, parameter :: NY = 4
  integer, parameter :: NZ = 4
  
  real, parameter :: INFINITY = huge(1.0)
  real, parameter :: SQRT_2   = sqrt(2.0)
  real, parameter :: SQRT_3   = sqrt(3.0)

  type :: vertex
     real :: u
     real :: tt
  end type vertex

  type(vertex), public,  allocatable ::        V(:,:,:)
  real,         private, allocatable ::       TT(:,:,:)
  logical,      public,  allocatable :: Finished(:,:,:)

contains

!-----------------------------------------------------------------------
! Initialize all the data structures in the module
!-----------------------------------------------------------------------
subroutine initialize()

   !! allocate space for halo region of 1 on each face of cube
   !
   allocate(       V(0:NX+1,0:NY+1,0:NZ+1))
   allocate(      TT(0:NX+1,0:NY+1,0:NZ+1))
   allocate(Finished(0:NX+1,0:NY+1,0:NZ+1))

   V%tt = INFINITY    
   V%u  = 1.0

   TT = INFINITY

   !! mark border (halo region) as finished, otherwise .not. finished
   !
   Finished = .FALSE.

   Finished( 0  , :  , :  ) = .TRUE.             !   left face
   Finished(NX+1, :  , :  ) = .TRUE.             !  right face
   Finished( :  , 0  , :  ) = .TRUE.             ! bottom face
   Finished( :  ,NY+1, :  ) = .TRUE.             !    top face
   Finished( :  , :  , 0  ) = .TRUE.             !  front face
   Finished( :  , :  ,NZ+1) = .TRUE.             !   back face

end subroutine initialize

!-----------------------------------------------------------------------
! Deallocate array memory previously allocated
!-----------------------------------------------------------------------
subroutine finalize()

   if (allocated(       V))  deallocate(       V)
   if (allocated(      TT))  deallocate(      TT)
   if (allocated(Finished))  deallocate(Finished)

end subroutine finalize

!-----------------------------------------------------------------------
! 
!-----------------------------------------------------------------------
function min_tt_unfinished(i0, j0, k0) result(done)
  integer, intent(out) :: i0, j0, k0
  logical :: done

  real    :: min_tt
  integer :: i, j, k

  !! mark as not found yet
  !
  i0 = 0;  j0 = 0;  k0 = 0

  where (.NOT. Finished)
     TT = V%tt
  else where
     TT = INFINITY
  end where

  min_tt = minval(TT)

!  do concurrent(i=1:NX, j=1:NY, k=1:NZ)
!  end do

  do k = 1, NZ
     do j = 1, NY
        do i = 1, NX
           if (.NOT. Finished(i,j,k)  .AND.  V(i,j,k)%tt == min_tt) then
              i0 = i;  j0 = j;  k0 = k
              done = .FALSE.
              return
           end if
        end do
     end do
  end do
  
  done = .TRUE.

end function min_tt_unfinished


!-----------------------------------------------------------------------
! 
!-----------------------------------------------------------------------
function min_tt_frontier(i, j, k) result(done)
  use frontier_list
  integer(C_INT), intent(out) :: i, j, k
  logical :: done

  real(C_DOUBLE) :: min_tt
  integer(C_INT) :: len

  call deleteMinNode(frontier, min_tt, i, j, k, len)

  if (len > 0) then
     done = .FALSE.
  else
     done = .TRUE.
  end if

end function min_tt_frontier


!-----------------------------------------------------------------------
! Relax the vertex given by the indices (i0,j0,k0) using loops
!-----------------------------------------------------------------------
subroutine relax_with_frontier(i0, j0, k0)
   use frontier_list
   implicit none
   integer, intent(in) :: i0, j0, k0

   !--- local variables ---
   !
   integer :: i, j, k
   real    :: t0, u0, dist(-1:1)
   double precision :: tt

   u0 = V(i0,j0,k0)%u          ! velocity at current vertex
   t0 = V(i0,j0,k0)%tt         ! travel time to current vertex

#if DEBUG > 1
   print *, "(u0,t0)", u0, t0
#endif

   !------------------- two vertical cube faces ----------------------
   !------------------------------------------------------------------
   do k = k0-1, k0+1, 2                         ! vertical cube faces
      do j = j0-1, j0+1                         ! horizontal lines
         if (j == j0) then
            dist = [SQRT_2,    1.0, SQRT_2]
         else
            dist = [SQRT_3, SQRT_2, SQRT_3]
         end if
         do i = i0-1, i0+1
            tt = t0 + 0.5*(u0 + V(i,j,k)%u)*dist(i-i0)
            if (tt < V(i,j,k)%tt  .AND.  .NOT. Finished(i,j,k)) then
               if (V(i,j,k)%tt == INFINITY) then
                  call Push(frontier, tt, i, j, k)
               else
                  call updateNodeTT(frontier, tt, i, j, k)
               end if
               V(i,j,k)%tt = tt
#if DEBUG > 1
               print *, i, j, k, dist(i-i0), tt, V(i,j,k)%tt
#endif
            end if
         end do
      end do
   end do

   !------------------- interior vertical slice ----------------------
   !------------------------------------------------------------------
   k = k0
   do j = j0-1, j0+1                         ! horizontal lines
      if (j == j0) then
         dist = [1.0,    0.0,    1.0]
      else
         dist = [SQRT_2, 1.0, SQRT_2]
      end if
      do i = i0-1, i0+1
         tt = t0 + 0.5*(u0 + V(i,j,k)%u)*dist(i-i0)
         if (tt < V(i,j,k)%tt  .AND.  .NOT. Finished(i,j,k)) then
            if (V(i,j,k)%tt == INFINITY) then
               call Push(frontier, tt, i, j, k)
            else
               call updateNodeTT(frontier, tt, i, j, k)
            end if
#if DEBUG > 1
            print *, i, j, k, dist(i-i0), tt, V(i,j,k)%tt
#endif
            V(i,j,k)%tt = tt
         end if
      end do
   end do

   !! mark this node as finished
   !
   Finished(i0,j0,k0) = .TRUE.
#if DEBUG > 0
   print *, "FINSHED:", i0, j0, k0, V(i0,j0,k0)%tt
#endif

end subroutine relax_with_frontier

!-----------------------------------------------------------------------
! Relax the vertex given by the indices (i0,j0,k0) using loops
!-----------------------------------------------------------------------
subroutine relax_loops(i0, j0, k0)
   implicit none
   integer, intent(in) :: i0, j0, k0

   !--- local variables ---
   !
   integer :: i, j, k
   real    :: t0, tt, u0, dist(-1:1)

   u0 = V(i0,j0,k0)%u          ! velocity at current vertex
   t0 = V(i0,j0,k0)%tt         ! travel time to current vertex

#if DEBUG > 1
   print *, "(u0,t0)", u0, t0
#endif

   !------------------- two vertical cube faces ----------------------
   !------------------------------------------------------------------
   do k = k0-1, k0+1, 2                         ! vertical cube faces
      do j = j0-1, j0+1                         ! horizontal lines
         if (j == j0) then
            dist = [SQRT_2,    1.0, SQRT_2]
         else
            dist = [SQRT_3, SQRT_2, SQRT_3]
         end if
         do i = i0-1, i0+1
            tt = t0 + 0.5*(u0 + V(i,j,k)%u)*dist(i-i0)
            if (tt < V(i,j,k)%tt) then
#if DEBUG > 1
               print *, i, j, k, dist(i-i0), tt, V(i,j,k)%tt
#endif
               V(i,j,k)%tt = tt
            end if
         end do
      end do
   end do

   !------------------- interior vertical slice ----------------------
   !------------------------------------------------------------------
   k = k0
   do j = j0-1, j0+1                         ! horizontal lines
      if (j == j0) then
         dist = [1.0,    0.0,    1.0]
      else
         dist = [SQRT_2, 1.0, SQRT_2]
      end if
      do i = i0-1, i0+1
         tt = t0 + 0.5*(u0 + V(i,j,k)%u)*dist(i-i0)
         if (tt < V(i,j,k)%tt) then
#if DEBUG > 1
            print *, i, j, k, dist(i-i0), tt, V(i,j,k)%tt
#endif
            V(i,j,k)%tt = tt
         end if
      end do
   end do

   !! mark this node as finished
   !
   Finished(i0,j0,k0) = .TRUE.
#if DEBUG > 0
   print *, "FINSHED:", i0, j0, k0, V(i0,j0,k0)%tt
#endif

end subroutine relax_loops

!-----------------------------------------------------------------------
! Relax the vertex given by the indices (i0,j0,k0) with loops unrolled
!-----------------------------------------------------------------------
  subroutine relax_unrolled(i, j, k, V)
    implicit none
    integer, intent(in) :: i, j, k
    type(vertex), intent(inout) :: V(0:,0:,0:)
    real :: t0, tt, u0

    u0 = V(i,j,k)%u          ! velocity at current vertex
    t0 = V(i,j,k)%tt         ! travel time to current vertex

    !----------------------- k vertical plane  ------------------------
    !------------------------------------------------------------------
    !----------------------- j horizontal line  -----------------------
    tt = t0 + 0.5*(u0 + V(i-1,j,k)%u)            ! i-1 edge
    if (tt < V(i-1,j,k)%tt) then
       V(i-1,j,k)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i+1,j,k)%u)            ! i+1 edge
    if (tt < V(i+1,j,k)%tt) then
       V(i+1,j,k)%tt = tt
    end if

    !----------------------- j-1 horizontal line ----------------------
    tt = t0 + 0.5*(u0 + V(i-1,j-1,k)%u)*SQRT_2   ! i-1 edge
    if (tt < V(i-1,j-1,k)%tt) then
       V(i-1,j-1,k)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i  ,j-1,k)%u)          ! i   edge
    if (tt < V(i,j-1,k)%tt) then
       V(i,j-1,k)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i+1,j-1,k)%u)*SQRT_2   ! i+1 edge
    if (tt < V(i+1,j-1,k)%tt) then
       V(i+1,j-1,k)%tt = tt
    end if

    !----------------------- j+1 horizontal line ----------------------
    tt = t0 + 0.5*(u0 + V(i-1,j+1,k)%u)*SQRT_2   ! i-1 edge
    if (tt < V(i-1,j+1,k)%tt) then
       V(i-1,j+1,k)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i  ,j+1,k)%u)          ! i   edge
    if (tt < V(i,j+1,k)%tt) then
       V(i,j+1,k)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i+1,j+1,k)%u)*SQRT_2   ! i+1 edge
    if (tt < V(i+1,j+1,k)%tt) then
       V(i+1,j+1,k)%tt = tt
    end if

    !----------------------- k-1 vertical plane -----------------------
    !------------------------------------------------------------------
    !----------------------- j horizontal line  -----------------------
    tt = t0 + 0.5*(u0 + V(i-1,j,k-1)%u)*SQRT_2     ! i-1 edge
    if (tt < V(i-1,j,k-1)%tt) then
       V(i-1,j,k-1)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i,j,k-1)%u)              ! i   edge
    if (tt < V(i-1,j,k-1)%tt) then
       V(i-1,j,k-1)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i+1,j,k-1)%u)*SQRT_2     ! i+1 edge
    if (tt < V(i+1,j,k-1)%tt) then
       V(i+1,j,k-1)%tt = tt
    end if

    !----------------------- j-1 horizontal line ----------------------
    tt = t0 + 0.5*(u0 + V(i-1,j-1,k-1)%u)*SQRT_3   ! i-1 edge
    if (tt < V(i-1,j-1,k-1)%tt) then
       V(i-1,j-1,k-1)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i  ,j-1,k-1)%u)*SQRT_2   ! i   edge
    if (tt < V(i,j-1,k-1)%tt) then
       V(i,j-1,k-1)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i+1,j-1,k-1)%u)*SQRT_3   ! i+1 edge
    if (tt < V(i+1,j-1,k-1)%tt) then
       V(i+1,j-1,k-1)%tt = tt
    end if

    !----------------------- j+1 horizontal line ----------------------
    tt = t0 + 0.5*(u0 + V(i-1,j+1,k-1)%u)*SQRT_3   ! i-1 edge
    if (tt < V(i-1,j+1,k-1)%tt) then
       V(i-1,j+1,k-1)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i  ,j+1,k-1)%u)*SQRT_2   ! i   edge
    if (tt < V(i,j+1,k-1)%tt) then
       V(i,j+1,k-1)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i+1,j+1,k-1)%u)*SQRT_3   ! i+1 edge
    if (tt < V(i+1,j+1,k-1)%tt) then
       V(i+1,j+1,k-1)%tt = tt
    end if

    !----------------------- k+1 vertical plane -----------------------
    !
    !----------------------- j horizontal line  -----------------------
    tt = t0 + 0.5*(u0 + V(i-1,j,k+1)%u)*SQRT_2     ! i-1 edge
    if (tt < V(i-1,j,k+1)%tt) then
       V(i-1,j,k+1)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i,j,k+1)%u)              ! i   edge
    if (tt < V(i-1,j,k+1)%tt) then
       V(i-1,j,k+1)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i+1,j,k+1)%u)*SQRT_2     ! i+1 edge
    if (tt < V(i+1,j,k+1)%tt) then
       V(i+1,j,k+1)%tt = tt
    end if

    !----------------------- j-1 horizontal line ----------------------
    tt = t0 + 0.5*(u0 + V(i-1,j-1,k+1)%u)*SQRT_3   ! i-1 edge
    if (tt < V(i-1,j-1,k+1)%tt) then
       V(i-1,j-1,k+1)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i  ,j-1,k+1)%u)*SQRT_2   ! i   edge
    if (tt < V(i,j-1,k+1)%tt) then
       V(i,j-1,k+1)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i+1,j-1,k+1)%u)*SQRT_3   ! i+1 edge
    if (tt < V(i+1,j-1,k+1)%tt) then
       V(i+1,j-1,k+1)%tt = tt
    end if

    !----------------------- j+1 horizontal line ----------------------
    tt = t0 + 0.5*(u0 + V(i-1,j+1,k+1)%u)*SQRT_3   ! i-1 edge
    if (tt < V(i-1,j+1,k+1)%tt) then
       V(i-1,j+1,k+1)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i  ,j+1,k+1)%u)*SQRT_2   ! i   edge
    if (tt < V(i,j+1,k+1)%tt) then
       V(i,j+1,k+1)%tt = tt
    end if
    tt = t0 + 0.5*(u0 + V(i+1,j+1,k+1)%u)*SQRT_3   ! i+1 edge
    if (tt < V(i+1,j+1,k+1)%tt) then
       V(i+1,j+1,k+1)%tt = tt
    end if

   !! mark this node as finished
   !
   Finished(i,j,k) = .TRUE.

  end subroutine relax_unrolled

end module dijkstra
