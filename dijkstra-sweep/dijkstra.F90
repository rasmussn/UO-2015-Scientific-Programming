#define DEBUG 1

module dijkstra
  implicit none

  !! dimensions for Joseph's example (101,161,51)
  !
  integer, parameter :: NX = 64
  integer, parameter :: NY = 64
  integer, parameter :: NZ = 64
  
  real, parameter :: INFINITY = huge(1.0)
  real, parameter :: SQRT_2   = sqrt(2.0)
  real, parameter :: SQRT_3   = sqrt(3.0)

  type :: vertex
     real :: u
     real :: tt
  end type vertex

  type(vertex), public,  allocatable :: V(:,:,:)

contains

!-----------------------------------------------------------------------
! Initialize all the data structures in the module
!-----------------------------------------------------------------------
subroutine initialize()

   !! allocate space for halo region of 1 on each face of cube
   !
   allocate( V(0:NX+1,0:NY+1,0:NZ+1) )

   V%tt = INFINITY    
   V%u  = 3.0
   if (NX > 24 .AND. NY > 24 .AND. NZ > 24) then
      V(8:24,8:24,8:24)%u = 1.0
   end if

end subroutine initialize

!-----------------------------------------------------------------------
! Deallocate array memory previously allocated
!-----------------------------------------------------------------------
subroutine finalize()

   if ( allocated(V) )  deallocate(V)

end subroutine finalize

!-----------------------------------------------------------------------
! Relax the vertex given by the indices (i0,j0,k0) using loops
!-----------------------------------------------------------------------
function relax(i0, j0, k0) result(changed)
   implicit none
   integer, intent(in) :: i0, j0, k0
   logical :: changed

   !--- local variables ---
   !
   integer :: i, j, k
   real    :: t0, tt, u0, dist(-1:1)

   changed = .FALSE.

   u0 = V(i0,j0,k0)%u          ! velocity at current vertex
   t0 = V(i0,j0,k0)%tt         ! travel time at current forward star point

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
            if (V(i,j,k)%tt < INFINITY) then
               tt = V(i,j,k)%tt + 0.5*(u0 + V(i,j,k)%u)*dist(i-i0)
               if (tt < t0) then
#if DEBUG > 1
                  print *, i, j, k, t0, tt, V(i,j,k)%tt
#endif
                  t0 = tt
                  changed = .TRUE.
               end if
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
         if (V(i,j,k)%tt < INFINITY) then
            tt = V(i,j,k)%tt + 0.5*(u0 + V(i,j,k)%u)*dist(i-i0)
            if (tt < t0) then
#if DEBUG > 1
               print *, i, j, k, t0, tt, V(i,j,k)%tt
#endif
               t0 = tt
               changed = .TRUE.
            end if
         end if
      end do
   end do

   if (changed) then
      V(i0,j0,k0)%tt = t0
   end if

end function relax

end module dijkstra
