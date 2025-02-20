program main
   use, intrinsic :: iso_fortran_env, only: output_unit
   use m_cli2, only: set_args, unnamed, sget, lget
   implicit none

   integer, parameter :: length = 89
   integer :: input, output, i
   integer :: line_num
   integer :: status = 0
   logical :: existed

   character(len=:), allocatable :: version_text(:), help_text(:)

   character(len=length) :: input_file, output_file
   character(len=length) :: buffer
   character(len=5) :: thisfmt

   write (thisfmt, '(a2, i2, a1)') '(a', length, ')'
   ! print *, thisfmt

   call setup()

   call set_args(" --output:o '' --number:n F", version_text=version_text, help_text=help_text)

   output_file = trim(sget("output"))

   if (len_trim(output_file) > 0) then
      open (file=output_file, newunit=output)
   else
      output = output_unit
   end if

   do i = 1, size(unnamed)
      input_file = trim(unnamed(i))

      inquire (file=input_file, exist=existed)

      if (existed) then
         write (output, *) "!", input_file

         line_num = 0

         open (newunit=input, file=input_file, access="sequential", status="old")

         do while (.true.)  ! 读取文件内容

            read (input, fmt=thisfmt, iostat=status) buffer

            if (status /= 0) exit  ! 没有数据就跳出循环

            if (lget("number")) then
               write (output, '(i3, a2) ', advance='no') line_num, "  "
               line_num = line_num + 1
            end if

            write (output, thisfmt) buffer

         end do

         close (input)
         write (output, *) ""

      else
         write (output_unit, *) trim(input_file), " doesn't exist."
      end if

   end do

   if (output /= output_unit) close (output)

contains

   subroutine setup()

      version_text = [character(len=80) :: "version 0.1.3", "author: soonsimple"]

      help_text = [character(len=80) :: "A Fortran port 'cat' ",&
      & "Usage: fcat -o output_file input_file1 input_file2 ...",&
      & "  --number:n display line numbers",&
      & "  --output:o output to file"]
   end subroutine setup

end program main
