program main
  use mpi

  implicit none
  integer :: it, it_total, num_proc
  logical :: compute_task=.false., io_task=.false.
  integer, dimension(4) :: array_local
  integer :: tag, dest, req
  ! valiables mpi
  integer :: ierr, color, rank_this, rank_total, io_start, comp_start
  integer :: global_comm, split_comm, compute_comm, ioserve_comm, inter_comm

  ! this test should be run with 5 procs (4 for compute and 1 for io server)
  num_proc = 5

  ! mpi initialization
  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD, rank_total, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank_this, ierr)

  ! check if the number of procs is ok
  if (rank_total /= num_proc ) then
    print *, "this program need to be run with nproc = ", num_proc
    print *, "input total rank = ", rank_total
    stop
  endif

  call mpi_comm_dup(MPI_COMM_WORLD, global_comm, ierr)

  ! Determine which ranks are compute tasks and which are I/O tasks
  ! If compute task, set color = 1
  ! If I/O task, set color = 0
  ! Assign one task per node to be an I/O task, round robin if necessary 
  ! set last proc as io server
  if (rank_this == rank_total-1) then
    ! for io server
    io_task=.true.
    color = 1
    print *, "io task taken by rank: ", rank_this
  else
    ! compute
    compute_task = .true.
    color = 0
    print *, "compute task taken by rank: ", rank_this
  endif

  call mpi_comm_split(global_comm, color, rank_this, split_comm, ierr)

  if (compute_task) call mpi_comm_dup(split_comm, compute_comm, ierr)
  if (io_task)      call mpi_comm_dup(split_comm, ioserve_comm, ierr)

  ! Create an intercommunicator between the compute comm and the IO comm.
  ! This allows us to send data from the compute world to the I/O world using
  ! MPI sends and receives.
  !MPI_Comm local_comm, int local_leader, MPI_Comm peer_comm, int remote_leader, int tag, MPI_Comm *newintercomm
  io_start = rank_total-1 ! unsure vals
  comp_start = 0 ! unsure vals
  if(compute_task) call mpi_intercomm_create(compute_comm, 0, global_comm, io_start, 0, inter_comm, ierr)
  if(io_task)      call mpi_intercomm_create(ioserve_comm, 0, global_comm, comp_start, 0, inter_comm, ierr)

  ! prepare test data
  if (compute_task) then
    array_local = (/rank_this,rank_this,rank_this,rank_this/)
    print *, "array_local initialized: ", array_local
  endif

  call mpi_barrier(MPI_COMM_WORLD, ierr)

  it_total = 5000
  ! time loop 
  do it = 1, it_total
    if (mod(it,1000) .eq. 0) then

      if (rank_this == 0) print *, "do io at : ", it
      if (compute_task) call do_work()

      ! Send my data to the I/O server, note non-blocking send
      if( compute_task ) then
        dest = 0
        tag  = 0
        print *, "sending local array: ", array_local
        call mpi_isend(array_local, size(array_local), MPI_INTEGER, dest, tag, inter_comm, req, ierr)
        !return
      end if

      if (io_task) call do_io(inter_comm, ioserve_comm, it, rank_this)

    endif

    ! sync at the last of each iteration
    call mpi_barrier(MPI_COMM_WORLD, ierr)
  enddo

  if (rank_this == 0) print *, "time iteration finished"
  call mpi_finalize(ierr)
end program main
