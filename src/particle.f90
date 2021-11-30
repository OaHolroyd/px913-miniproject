program particle
  use model_data
  use read_input

  implicit none

  integer :: err ! error flag

  ! read and validate command line args
  call init_args(err)
  if (err /= 0) stop
  call validate_args(err)
  if (err /= 0) stop

  print*, nx
  print*, ny
  print*, problem

  call init_arrays()

end program particle
