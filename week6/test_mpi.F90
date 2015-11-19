program test_mpi
  use mpi_f08
  implicit none

  integer, parameter :: TAG  = 13
  integer, parameter :: ROOT = 0
  integer, parameter :: MAX_WORKERS = 16
  integer, parameter :: CMD_WORK = 1, CMD_COMPLETED = -1, CMD_EXIT = -2

  integer :: rank, size, index, worker, work(2)
  logical :: finished
  type(MPI_Status)  :: status
  type(MPI_Request) :: requests(MAX_WORKERS)

  call MPI_Init()
  call MPI_Comm_rank(MPI_COMM_WORLD, rank)
  call MPI_Comm_size(MPI_COMM_WORLD, size)

  if (size >= MAX_WORKERS) then
     if (rank == ROOT) print *, "ERROR: request for too many workers, MAX is", MAX_WORKERS
     call MPI_Finalize()
     STOP
  end if

  print ("('[', i2, ']:', 1x, 'size:', i3)"), rank, size

  call MPI_Barrier(MPI_COMM_WORLD)
  if (rank == ROOT) print *, "---------------------------------------"

  finished = .FALSE.
  do while (.NOT. finished)

     if (rank /= ROOT) then

        !! Order of events for workers
        !
        !  1. Recv work-list [CMD, index]
        !     a. if CMD==CMD_EXIT, exit, otherwise continue
        !  2. Compute travel times for source associated with index
        !  3. Write results for source associated with index
        !  4. Send READY command back to root process
        !  5. Loop back to step 1.
        !
        call MPI_Recv(work, 2, MPI_INTEGER, ROOT, TAG, MPI_COMM_WORLD, status)
        print ("('[', i2, ']:', 1x, 'recv:', i3, i3)"), rank, work(1), work(2)
        if (work(1) == CMD_EXIT) then
           print ("('[', i2, ']:', 1x, 'recv:', i3, i3, ' shutting down')"), rank, work(1), work(2)
           finished = .TRUE.
        end if

        ! report back
        work(1) = CMD_COMPLETED
        call MPI_Send(work, 2, MPI_INTEGER, ROOT, TAG, MPI_COMM_WORLD)

     else
        !! Order of events for root process
        !
        !  1. Send work [CMD, index] to each worker
        !  2. Poll workers to see if they need more work
        !  3. Send work to a worker needing work
        !     a. if no more work, send CMD==CMD_EXIT to worker
        !  4. Loop back to step 2
        !

        ! send work to workers
        do worker = 1, size-1
           index = worker   ! for now, for testing
           work = [CMD_WORK, index]
           print ("('[', i2, ']:', 1x, 'send:', i3, i3, ' to', i3)"), rank, work(1), work(2), worker
           call MPI_Send(work, 2, MPI_INTEGER, worker, TAG, MPI_COMM_WORLD)
        end do

        ! recv completed status from workers
        do worker = 1, size-1
           index = worker   ! for now, for testing
           work = [CMD_EXIT, index]
           print ("('[', i2, ']:', 1x, 'send:', i3, i3, ' to', i3)"), rank, work(1), work(2), worker
           call MPI_Recv(work, 2, MPI_INTEGER, worker, TAG, MPI_COMM_WORLD, status)
        end do

        ! send shutdown command
        do worker = 1, size-1
           index = worker   ! for now, for testing
           work = [CMD_EXIT, index]
           print ("('[', i2, ']:', 1x, 'send:', i3, i3, ' to', i3)"), rank, work(1), work(2), worker
           call MPI_Send(work, 2, MPI_INTEGER, worker, TAG, MPI_COMM_WORLD)
        end do

        print *, "---------------------------------------"
        print ("('[', i2, ']:', 1x, 'root:', i3, i3, ' shutting down')"), rank, work(1), work(2)
        finished = .TRUE.
     end if

  end do

  call MPI_Finalize()

end program
