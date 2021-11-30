program particle
  use command_line

  implicit none

  integer :: nx, ny
  character(len=8) :: problem

  ! Default arguments
  nx = 10
  ny = 15
  problem = "null"

  ! Parse all the command line args and read them
  CALL parse_args()

  IF (.NOT.get_arg("nx", nx)) THEN
    PRINT"(A, I0)", "WARNING: argument 'nx' not received, defaulting to ", nx
  ENDIF

  IF (.NOT.get_arg("ny", ny)) THEN
    PRINT"(A, I0)", "WARNING: argument 'ny' not received, defaulting to ", ny
  ENDIF

  IF (.NOT.get_arg("problem", problem)) THEN
    PRINT"(A, A)", "WARNING: argument 'init' not received, defaulting to ", problem
  ENDIF

end program particle
