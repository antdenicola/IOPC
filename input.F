      subroutine input
      use varsh
      implicit none
      
      write(6,*) '> Insert the total number of molecules: '
      read(5,*) nmol

      write(6,*) '> Insert the total number of particles: '
      read(5,*) natom

      write(6,*) '> Insert the total number of molecule A: '
      read(5,*) molA

      write(6,*) '> Insert the number of particles in molecule A: '
      read(5,*) npartA

      write(6,*) '> Insert the total number of molecule B: '
      read(5,*) molB

      write(6,*) '> Insert the number of particles in molecule B: '
      read(5,*) npartB

      write(6,*) '> Insert the total number of molecule C: '
      read(5,*) molC

      write(6,*) '> Insert the number of particles in molecule C: '
      read(5,*) npartC

      write(6,*) '> Insert the total number of molecule D: '
      read(5,*) molD

      write(6,*) '> Insert the number of particles in molecule D: '
      read(5,*) npartD

      write(6,*) '> Velocity read [1] yes, [2] no: '
      read(5,*) vmode

      write(6,*) '> Insert the number of CPU: '
      read(5,*) numprocs

      write(6,*) '----------------------------------------'


      allocate(nat(nmol))
      allocate(itype(natom))
      allocate(nbonds(natom)) 
      allocate(ibond(natom,6))
      allocate(ibond1(natom,6))
      allocate(molApara(numprocs)) 
      allocate(molBpara(numprocs))
      allocate(molCpara(numprocs))
      allocate(molDpara(numprocs))
      allocate(Ntotalpara(numprocs)) 
      
      allocate(x(natom))
      allocate(y(natom))
      allocate(z(natom))
      allocate(vx(natom))
      allocate(vy(natom))
      allocate(vz(natom))
      allocate(clab(natom))

      end subroutine
