module velocity_verlet

  use model_data
    
  implicit none
  save!?
    
  contains
    
  subroutine move_particle()

    cell_x = floor((particle_one%pos(1,0) + 1.0_dp)/dx) +1!determining which cell the particle is in, to use only that cell's field
    cell_y = floor((particle_one%pos(2,0) + 1.0_dp)/dy) +1
    particle_one%acc(1,0) = (q/m)*ex(cell_x, cell_y)
    particle_one%acc(2,0) = (q/m)*ey(cell_x, cell_y)

    !loop through timesteps
    do count = 1,num_of_timesteps
      particle_one%pos(:,count) = particle_one%pos(:,count-1) + dt*particle_one%vel(:,count-1)&
      + 0.5_dp*particle_one%acc(:,count-1)*dt*dt
    
      cell_x = floor((particle_one%pos(1,count) + 1.0_dp)/dx) +1 !determining which cell the particle is in, to use only that cell's field
      cell_y = floor((particle_one%pos(2,count) + 1.0_dp)/dy) +1
      particle_one%acc(1,count) = (q/m)*ex(cell_x, cell_y)
      particle_one%acc(2,count) = (q/m)*ey(cell_x, cell_y)
    
      particle_one%vel(:,count) = particle_one%vel(:,count-1) + 0.5_dp*dt*(particle_one%acc(:,count) + &
      particle_one%acc(:,count-1))

      if (cell_x >nx .or. cell_x <1 .or. cell_y >ny .or. cell_y <1) then !exit loop if leaves box
        exit
      end if
    end do   

  end subroutine
end module