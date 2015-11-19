module frontier_list
   use, intrinsic :: ISO_C_BINDING
   implicit none

   type Node
      real(C_DOUBLE) :: tt
      integer(C_INT) :: i, j, k
      type(C_PTR) :: next
   end type Node

   type(C_PTR) :: frontier = C_NULL_PTR

   interface
      subroutine Head(list, aNode) &
         bind(C, name="Head")
         use, intrinsic :: ISO_C_BINDING
         import :: Node
         type(C_PTR), intent(inout) :: list
         type(Node), intent(out)    :: aNode
      end subroutine Head

      subroutine Push(list, tt, i, j, k) &
         bind(C, name="Push")
         use, intrinsic :: ISO_C_BINDING
         type(C_PTR), intent(inout)        :: list
         real(C_DOUBLE), intent(in), value :: tt
         integer(C_INT), intent(in), value :: i, j, k
      end subroutine Push

      subroutine updateNodeTT(list, newTT, i, j, k) &
         bind(C, name="updateNodeTT")
         use, intrinsic :: ISO_C_BINDING
         type(C_PTR), intent(inout)        :: list
         real(C_DOUBLE), intent(in), value :: newTT
         integer(C_INT), intent(in), value :: i, j, k
       end subroutine updateNodeTT

       subroutine deleteMinNode(list, tt, i, j, k, len) &
         bind(C, name="deleteMinNode")
         use, intrinsic :: ISO_C_BINDING
         type(C_PTR), intent(inout)  :: list
         real(C_DOUBLE), intent(out) :: tt
         integer(C_INT), intent(out) :: i, j, k, len
       end subroutine deleteMinNode

       function getLength(list) result(length) &
         bind(C, name="getLength")
         use, intrinsic :: ISO_C_BINDING
         type(C_PTR), intent(in), value :: list
         integer(C_INT)                 :: length
       end function getLength

   end interface

end module frontier_list
