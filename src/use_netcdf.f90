module write_netcdf

  use iso_fortran_env
  use model_data
  use netcdf

  implicit none
  
  contains
  !!!subroutine written by Heather, based on online example*, then built upon by Matt
  !!!*https://web.archive.org/web/20190623025346/http://people.sc.fsu.edu/~jburkardt/f_src/netcdf/netcdf.html
  subroutine writer() 

    integer, dimension(2) :: particle_sizes, particle_dim_ids
    character(len=1), dimension(2) :: particle_dims=(/"u", "t"/) !For position, velocity and acceleration 

    integer, dimension(2) :: one_to_nx_sizes, one_to_nx_ids
    character(len=1), dimension(2) :: one_to_nx_dims=(/"x", "y"/) !For rho, phi, Ex and Ey

    integer :: file_id, pos_id, vel_id, acc_id, i, rho_id, phi_id, ex_id, ey_id

    !removing ghost cells as no longer needed
    rho = rho(1:nx,1:ny) 
    phi = phi(1:nx,1:ny) 

    particle_sizes = shape(particle_one%pos) !Position, velocity and acceleration all have same dimensions
    one_to_nx_sizes = shape(rho) !rho, phi, Ex and Ey all have same dimensions

    ! create the file, overwriting if it exists
    ierr = nf90_create(filename, nf90_clobber, file_id)

    ! i don't want to bomb if there is an error, rather return to caller
    ! this is tricky to do from another sub. so i choose this instead
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ! now i am going to do several actions, checking and printing each one
    ! i am using a loop here, to save a bit of duplication. in higher
    ! dimensions, this would really help!

    do i = 1, 2
      ierr = nf90_def_dim(file_id, particle_dims(i), particle_sizes(i), particle_dim_ids(i))
      if (ierr /= nf90_noerr) then
        print*, trim(nf90_strerror(ierr))
        return
      end if
    end do

    do i = 1, 2
      ierr = nf90_def_dim(file_id, one_to_nx_dims(i), one_to_nx_sizes(i), one_to_nx_ids(i))
      if (ierr /= nf90_noerr) then
        print*, trim(nf90_strerror(ierr))
        return
      end if
    end do

    !global attribute: title - more could be added here if desired.
    ierr = nf90_put_att(file_id, nf90_global, "title", "Particle in an electric field")
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ! define variable types, matching our array

    ierr = nf90_def_var(file_id, "position", nf90_double, particle_dim_ids, pos_id)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ierr = nf90_def_var(file_id, "velocity", nf90_double, particle_dim_ids, vel_id)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ierr = nf90_def_var(file_id, "acceleration", nf90_double, particle_dim_ids, acc_id)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ierr = nf90_def_var(file_id, "rho", nf90_double, one_to_nx_ids, rho_id)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ierr = nf90_def_var(file_id, "phi", nf90_double, one_to_nx_ids, phi_id)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ierr = nf90_def_var(file_id, "Ex", nf90_double, one_to_nx_ids, ex_id)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ierr = nf90_def_var(file_id, "Ey", nf90_double, one_to_nx_ids, ey_id)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ! finish defining metadata
    ierr = nf90_enddef(file_id)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ! actually writing the variables

    ierr = nf90_put_var(file_id, pos_id, particle_one%pos)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ierr = nf90_put_var(file_id, vel_id, particle_one%vel)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ierr = nf90_put_var(file_id, acc_id, particle_one%acc)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ierr = nf90_put_var(file_id, rho_id, rho)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ierr = nf90_put_var(file_id, phi_id, phi)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ierr = nf90_put_var(file_id, ex_id, ex)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ierr = nf90_put_var(file_id, ey_id, ey)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

    ! close the file
    ierr = nf90_close(file_id)
    if (ierr /= nf90_noerr) then
      print*, trim(nf90_strerror(ierr))
      return
    end if

  end subroutine writer

end module write_netcdf
