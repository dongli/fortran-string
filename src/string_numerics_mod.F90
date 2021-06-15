module string_numerics_mod

  implicit none

  private

  public to_str
  public to_int
  public to_r4
  public to_r8

  interface to_str
    module procedure i1_to_str
    module procedure i2_to_str
    module procedure i4_to_str
    module procedure i8_to_str
    module procedure i4_array_to_str
    module procedure r4_to_str
    module procedure r8_to_str
    module procedure r8_array_to_str
    module procedure l_to_str
  end interface to_str

contains

  pure function i1_to_str(x) result(res)

    integer(1), intent(in) :: x
    character(:), allocatable :: res

    character(range(x)+2) tmp

    write(tmp, '(i0)') x
    res = trim(tmp)

  end function i1_to_str
  
  pure function i2_to_str(x) result(res)

    integer(2), intent(in) :: x
    character(:), allocatable :: res

    character(range(x)+2) tmp

    write(tmp, '(i0)') x
    res = trim(tmp)

  end function i2_to_str
  
  pure function i4_to_str(x, pad_zeros) result(res)

    integer(4), intent(in) :: x
    integer, intent(in), optional :: pad_zeros
    character(:), allocatable :: res

    character(range(x)+2) tmp
    character(256) fmt

    if (present(pad_zeros)) then
      if (pad_zeros > 0) then
        write(fmt, '("(i0.", i0, ")")') pad_zeros
        write(tmp, fmt) x
      else
        write(tmp, '(i0)') x
      end if
    else
      write(tmp, '(i0)') x
    end if
    res = trim(tmp)

  end function i4_to_str
  
  pure function i8_to_str(x) result(res)

    integer(8), intent(in) :: x
    character(:), allocatable :: res

    character(range(x)+2) tmp

    write(tmp, '(i0)') x
    res = trim(tmp)

  end function i8_to_str

  pure function i4_array_to_str(x) result(res)

    integer(4), intent(in) :: x(:)
    character(:), allocatable :: res

    character((range(x)+4) * size(x)) tmp
    character(256) fmt

    write(fmt, '("(", I0, "(I0, "", ""))")') size(x)
    write(tmp, fmt) x
    res = trim(tmp)

  end function i4_array_to_str

  pure function r4_to_str(x, decimal_width, width) result(res)

    real(4), intent(in) :: x
    integer, intent(in) :: decimal_width
    integer, intent(in), optional :: width
    character(:), allocatable :: res

    integer w
    character(10) fmt
    character(range(x)+2) tmp1, tmp2

    if (present(width)) then
      w = max(width, decimal_width + 1 + 6)
      write(fmt, "('(G', I0, '.', I0, ')')") w, decimal_width + 1
      write(tmp1, fmt) x
    else
      write(tmp1, "(I0)") int(x)
      write(tmp2, "(I0)") abs(int(x * 10**decimal_width) - int(x) * 10**decimal_width)
      tmp1 = trim(tmp1) // '.' // trim(adjustl(tmp2))
    end if
    res = trim(adjustl(tmp1))

  end function r4_to_str

  pure function r8_to_str(x, decimal_width, width) result(res)

    real(8), intent(in) :: x
    integer, intent(in) :: decimal_width
    integer, intent(in), optional :: width
    character(:), allocatable :: res

    integer w
    character(10) fmt
    character(range(x)+2) tmp1, tmp2

    if (present(width)) then
      w = max(width, decimal_width + 1 + 6)
      write(fmt, "('(G', I0, '.', I0, ')')") w, decimal_width + 1
      write(tmp1, fmt) x
    else
      write(tmp1, "(I0)") int(x)
      write(tmp2, "(I0)") abs(int(x * 10**decimal_width) - int(x) * 10**decimal_width)
      tmp1 = trim(tmp1) // '.' // trim(adjustl(tmp2))
    end if
    res = trim(adjustl(tmp1))

  end function r8_to_str

  pure function r8_array_to_str(x, decimal_width, width) result(res)

    real(8), intent(in) :: x(:)
    integer, intent(in) :: decimal_width
    integer, intent(in), optional :: width
    character(:), allocatable :: res

    integer w, i, j
    character(256) s
    character(:), allocatable :: tmp

    if (present(width)) then
      w = max(width, decimal_width + 1 + 5)
    else
      w = decimal_width + 1 + 5
    end if
    allocate(character(2+(w+1)*size(x)-1)::tmp)
    tmp(2:len(tmp)) = ''
    tmp(1:1) = '['
    j = 2
    do i = 1, size(x)
      s = to_str(x(i), decimal_width, width)
      tmp(j:j+len_trim(s)-1) = trim(s)
      j = j + len_trim(s)
      if (i /= size(x)) then
        write(tmp(j:j), '(",")')
        j = j + 1
      end if
    end do
    tmp(j:j) = ']'
    res = trim(tmp)
    deallocate(tmp)

  end function r8_array_to_str

  pure function l_to_str(x) result(res)

    logical, intent(in) :: x
    character(:), allocatable :: res

    res = trim(merge('true ', 'false', x))

  end function l_to_str

  pure integer function to_int(x) result(res)

    character(*), intent(in) :: x

    read(x, *) res

  end function to_int

  pure real(4) function to_r4(x) result(res)

    character(*), intent(in) :: x

    read(x, *) res

  end function to_r4

  pure real(8) function to_r8(x) result(res)

    character(*), intent(in) :: x

    read(x, *) res

  end function to_r8

end module string_numerics_mod
