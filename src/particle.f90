program particle
  use model_data
  use read_input
  use gauss_seidel

  implicit none

  integer :: err ! error flag

  ! read and validate command line args
  call init_args(err)
  if (err /= 0) stop
  call validate_args(err)
  if (err /= 0) stop

  ! print input to check
  print"(A, I0)", "nx: ", nx
  print"(A, I0)", "ny: ", ny
  print"(A, A)", "problem: ", problem

  ! allocate space and generate the initial data
  call init_model_data()

  ! generate the scalar potential
  call generate_phi()
  call generate_e()

end program particle
