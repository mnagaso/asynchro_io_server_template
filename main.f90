program main
  use mpi

  implicit none

  integer                        :: it, it_total, dur_io, num_proc, num_io, num_compute
  logical                        :: compute_task=.false., io_task=.false.
  integer, dimension(2,2)        :: array_local ! test sending multi array
  double precision, dimension(4) :: array_local2
  integer                        :: tag1, tag2, dest, req, io_id, num_total_msg

  ! valiables mpi
  integer :: ierr, color, rank_this, rank_total, io_start, comp_start
  integer :: global_comm, split_comm, compute_comm, ioserve_comm, inter_comm

  ! this test should be run with 6 procs (4 for compute and 2 for io server)
  num_proc    = 6               ! number of all procs (compute nodes+io nodes)
  num_io      = 2               ! number of io nodes
  num_compute = num_proc-num_io ! number of compute nodes

  ! mpi tags are used to distinguish the type of data which io nodes receive.
  tag1 = 1111
  tag2 = 2222

  ! setup timesteps and io timing
  it_total = 5000
  dur_io   = 1000 ! each dur_io timesteps, io will be done
  num_total_msg = int(it_total/dur_io)*2 ! 2 is the number of arrays to be sent
  if(rank_this==0) print *, "it_total:", it_total, "io_gap:", dur_io, "total msg on each io node:",num_total_msg

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

  ! Determine which ranks are compute tasks and which are I/O tasks
  ! If compute task, set color = 1
  ! If I/O task, set color = 0
  ! Assign one task per node to be an I/O task, round robin if necessary 
  ! set last proc as io server
  if (rank_this >= rank_total-num_io) then
    ! for io server
    io_task=.true.
    color = 1
    io_id = rank_this-num_compute
    print *, "io task taken by rank: ", rank_this
  else
    ! compute
    compute_task = .true.
    color = 0
    
    ! set the destination id of io node
    if (rank_this < 2) then
      dest = 0
    else 
      dest = 1
    endif
    
    print *, "compute task taken by rank: ", rank_this
  endif

  call mpi_comm_dup(MPI_COMM_WORLD, global_comm, ierr)
  call mpi_comm_split(global_comm, color, rank_this, split_comm, ierr)

  ! duplication of communicator can be skipped but done here to make the code clearer
  ! when skipping this duplicatoinm mpi_intercomm_create are called with split_comm
  if (compute_task) call mpi_comm_dup(split_comm, compute_comm, ierr)
  if (io_task)      call mpi_comm_dup(split_comm, ioserve_comm, ierr)

  ! Create an intercommunicator between the compute comm and the IO comm.
  ! This allows us to send data from the compute world to the I/O world using
  ! MPI sends and receives.
  io_start   = num_compute ! local reader of io group
  comp_start = 0           ! local reader of compute group

  if(compute_task) call mpi_intercomm_create(compute_comm, 0, global_comm, io_start,   0, inter_comm, ierr)
  if(io_task)      call mpi_intercomm_create(ioserve_comm, 0, global_comm, comp_start, 0, inter_comm, ierr)

  ! prepare test data arrays
  if (compute_task) then
    array_local = reshape((/rank_this+10,rank_this+20,rank_this+30,rank_this+40/), shape(array_local))
    print *, "array_local initialized: ", array_local

    array_local2 = (/rank_this*0.01,rank_this*0.001,rank_this*0.0001,rank_this*0.00001/)
    print *, "array_local2 initialized: ", array_local2
  endif

  ! synchronize before starting time iteration
  call mpi_barrier(global_comm, ierr)
 
  ! start idling of io node
  if (io_task) call idle_io(inter_comm, ioserve_comm, io_id, num_total_msg)
  
  ! iterate loop will be done by only the compute nodes
  if (compute_task) then

    ! time loop 
    do it = 0, it_total

      ! write output at each 1000 time steps
      if (mod(it,dur_io) .eq. 0) then

        ! indicate the current timestep
        if (rank_this == 0) print *, "############## io at : ", it, "################"

        ! ...

        call do_work() ! call subroutines for only compute nodes like this

        ! ...

        ! Send my data to the I/O server, note non-blocking send
        ! first array
        call mpi_isend(array_local, size(array_local), MPI_INTEGER, dest, tag1, inter_comm, req, ierr)

        ! second array
        call mpi_isend(array_local2, size(array_local2), MPI_DOUBLE, dest, tag2, inter_comm, req, ierr)

      endif

    enddo

  endif ! if compute_task


  if (rank_this == 0) print *, "time iteration finished"
  call mpi_finalize(ierr)

end program main
