! Keep this grammar in sync with Radio::is_complete_cb_callsign.
! These predicates replace amateur-only plausibility checks, never CRC checks.
module cb_callsigns
  implicit none
  private
  public :: is_complete_cb_callsign, is_cb_type4_message
contains
  pure logical function is_complete_cb_callsign(text) result(valid)
    character(len=*), intent(in) :: text
    valid=is_legacy_cb_callsign(text)
    if(.not.valid) valid=is_extended_cb_callsign(text)
  end function

  pure logical function is_legacy_cb_callsign(text) result(valid)
    character(len=*), intent(in) :: text
    character(len=len(text)) :: call
    integer :: i,n,prefix,letters,suffix,k
    valid=.false.
    call=trim(adjustl(text))
    n=len_trim(call)
    do i=1,n
      k=iachar(call(i:i))
      if(k>=iachar('a') .and. k<=iachar('z')) call(i:i)=achar(k-32)
    enddo
    i=1
    do while(i<=n)
      if(call(i:i)<'0' .or. call(i:i)>'9') exit
      i=i+1
    enddo
    prefix=i-1
    if(prefix<1 .or. prefix>3) return
    do while(i<=n)
      if(call(i:i)<'A' .or. call(i:i)>'Z') exit
      i=i+1
    enddo
    letters=i-prefix-1
    if(letters<1 .or. letters>2 .or. i>n) return
    if(call(i:i)=='/') then
      if(n-i/=2) return
      valid=all_letters(call(i+1:n))
    else
      suffix=n-i+1
      if(suffix>3 .and. .not.(prefix==1 .and. suffix==4)) return
      do k=i,n
        if(call(k:k)<'0' .or. call(k:k)>'9') return
      enddo
      valid=.true.
    endif
  end function

  ! Extended CB grammar (must stay in sync with Radio.cpp, extended_cb_match):
  !   call = A + B + C, total length 3..11, characters 0-9 A-Z only
  !   A : starts with a digit, 1-3 digits and 0-2 letters (any order)
  !   B : starts and ends with a letter, 1-3 letters and 0-2 digits
  !   C : 1-2 characters, each a digit or a letter
  ! Digit-leading amateur-style calls  d L{1,2} d L{0,3}  (9A1AA, 3DA0XX...)
  ! are never CB calls, so they keep their amateur handling.
  pure logical function is_extended_cb_callsign(text) result(valid)
    character(len=*), intent(in) :: text
    character(len=len(text)) :: call
    integer :: i,j,n,k
    valid=.false.
    call=trim(adjustl(text))
    n=len_trim(call)
    if(n<3 .or. n>11) return
    do i=1,n
      k=iachar(call(i:i))
      if(k>=iachar('a') .and. k<=iachar('z')) call(i:i)=achar(k-32)
      if(.not.(is_dig(call(i:i)) .or. is_let(call(i:i)))) return
    enddo
    if(.not.is_dig(call(1:1))) return
    if(is_amateur_like(call(1:n))) return
    do i=1,n-2                       ! A = call(1:i)
      if(.not.seg_a(call(1:i))) cycle
      do j=i+1,n-1                   ! B = call(i+1:j), C = call(j+1:n)
        if(n-j>2) cycle
        if(seg_b(call(i+1:j))) then
          valid=.true.
          return
        endif
      enddo
    enddo
  end function

  pure logical function is_dig(c) result(r)
    character(len=1), intent(in) :: c
    r=(c>='0' .and. c<='9')
  end function

  pure logical function is_let(c) result(r)
    character(len=1), intent(in) :: c
    r=(c>='A' .and. c<='Z')
  end function

  pure logical function seg_a(s) result(r)
    character(len=*), intent(in) :: s
    integer :: i,nd,nl
    r=.false.
    if(len(s)<1) return
    if(.not.is_dig(s(1:1))) return
    nd=0; nl=0
    do i=1,len(s)
      if(is_dig(s(i:i))) then; nd=nd+1; else; nl=nl+1; endif
    enddo
    r=(nd>=1 .and. nd<=3 .and. nl<=2)
  end function

  pure logical function seg_b(s) result(r)
    character(len=*), intent(in) :: s
    integer :: i,nd,nl
    r=.false.
    if(len(s)<1) return
    if(.not.is_let(s(1:1)) .or. .not.is_let(s(len(s):len(s)))) return
    nd=0; nl=0
    do i=1,len(s)
      if(is_dig(s(i:i))) then; nd=nd+1; else; nl=nl+1; endif
    enddo
    r=(nl>=1 .and. nl<=3 .and. nd<=2)
  end function

  ! d L{1,2} d L{0,3}
  pure logical function is_amateur_like(s) result(r)
    character(len=*), intent(in) :: s
    integer :: i,n,nl1,nl2
    r=.false.
    n=len(s)
    if(n<3) return
    if(.not.is_dig(s(1:1))) return
    i=2; nl1=0
    do while(i<=n)
      if(.not.is_let(s(i:i))) exit
      nl1=nl1+1; i=i+1
    enddo
    if(nl1<1 .or. nl1>2 .or. i>n) return
    if(.not.is_dig(s(i:i))) return
    i=i+1; nl2=0
    do while(i<=n)
      if(.not.is_let(s(i:i))) return
      nl2=nl2+1; i=i+1
    enddo
    r=(nl2<=3)
  end function

  pure logical function all_letters(text) result(valid)
    character(len=*), intent(in) :: text
    integer :: i
    valid=.false.
    do i=1,len(text)
      if(text(i:i)<'A' .or. text(i:i)>'Z') return
    enddo
    valid=.true.
  end function

  pure logical function hashed_cb(text) result(valid)
    character(len=*), intent(in) :: text
    integer :: n
    n=len_trim(text)
    valid=text=='<...>'
    if(n<3) return
    if(text(1:1)=='<' .and. text(n:n)=='>') then
      valid=valid .or. is_complete_cb_callsign(text(2:n-1))
    endif
  end function

  pure logical function is_cb_type4_message(text) result(valid)
    character(len=*), intent(in) :: text
    character(len=len(text)) :: first,second,tail
    integer :: i,j
    valid=.false.
    i=index(trim(text),' ')
    if(i<2) return
    first=text(:i-1)
    tail=adjustl(text(i+1:))
    if(first=='CQ') then
      valid=is_complete_cb_callsign(tail)
      return
    endif
    j=index(trim(tail),' ')
    second=tail
    if(j>0) then
      second=tail(:j-1)
      tail=adjustl(tail(j+1:))
      if(tail/='RRR' .and. tail/='RR73' .and. tail/='73') return
    endif
    ! A type 4 QSO carries exactly one full call and one hashed call.
    valid=(is_complete_cb_callsign(first) .and. hashed_cb(second)) .or. &
          (hashed_cb(first) .and. is_complete_cb_callsign(second))
  end function
end module
