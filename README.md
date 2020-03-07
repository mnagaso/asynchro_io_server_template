# asynchro_io_server_template

This is the runnable example of asynchronous IO server implementation written in fortran.

main.f90: main function for initialization, mpi process separation, time iteration.  
subrountine_ex.f90: examples of subroutines for idling io server, other call to do calculation.

Detailed explanation is in the comment lines of the codes.

To make and run:  
```bash
./make_and_run_test.sh
```




Implemented refering [this slide](https://www.cscs.ch/fileadmin/user_upload/contents_publications/tutorials/fast_parallel_IO/SimpleAsyncIOServer_MC.pdf).
