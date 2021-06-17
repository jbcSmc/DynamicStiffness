!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This file is part of DynamicSiffness, a Fortran library that         ! 
! implements the Dynamic Stiffness Method                              !
! Copyright (C) 2018  Jean-Baptiste CASIMIR,                           !
! Quartz Laboratory - Supmeca                                          !
! 3 rue Ferand Hainaut                                                 !
! 93407 SAINT-OUEN - FRANCE                                            !
! jean-baptiste.casimir@supmeca.fr                                     !
!                                                                      !
! This program is free software: you can redistribute it and/or modify !
! it under the terms of the GNU General Public License as published by !
! the Free Software Foundation, either version 3 of the License, or    !
! (at your option) any later version.                                  !
!                                                                      !
! This program is distributed in the hope that it will be useful,      !
! but WITHOUT ANY WARRANTY; without even the implied warranty of       !
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        !
! GNU General Public License for more details.                         !
!                                                                      !
! You should have received a copy of the GNU General Public License    !
! along with this program.  If not, see <http://www.gnu.org/licenses/>.!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     This program is a fortran90 example for using the fortran library!
!     DynamicStiffness with the gtk-fortran graphical library          !
!                                                                      !
!     This program computes the harmonic response of a 2D beam         !
!     structure described in a data file (see ReadDataFile.for)        !
!                                                                      !  
!     A harmonic unit force is applied on a chosen DOF and the harmonic!
!     response is computed in the direction of another DOF             !
!                                                                      !      
!     The solutions are obtained thanks to a Lapack's procedure that   ! 
!     solves the symmetric linear algebraic system (ZSYSV)             !    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Shared types and variables                                           !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module common_Dsm
   use iso_c_binding
         
!  Drawing area widget and its dimensions are global                   !   
   type(c_ptr) :: draw
   integer(kind=c_int)::width,height
   
!  The leading dimensions of static arrays that describe the structure ! 
   integer nmax,emax,smax,mmax,readOk
   parameter (nmax=100,emax=100,smax=100,mmax=100)

!  Type Structure                                                      !
!  nodes : Node coordinates                                            !
!  mates : Materials   (see ReadDataFiles)                             !  
!  sects : Sections    (see ReadDataFiles)                             !
!  elems : Elements' connectivities and properties                     !
!  nn : The number of nodes                                            !
!  ne : The number of elements                                         !
!  ns : The number of sections                                         !
!  nm : The number of materials                                        !
!  cat : Category of problem (only 2DFRAME)                            !
!  xmin, xmax, ymin, ymax : extremal dimensions of the structure       !

!Modification of structure type for Timoshenko : 04/2021			   !
   type Structure
	  double precision nodes(nmax,2),mates(mmax,4),sects(smax,3)
      integer elems(emax,5)
      integer nn,ne,nm,ns
      character*7 cat
      double precision xmin,xmax,ymin,ymax
   end type structure
end module common_Dsm

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Widgets of the interface                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module dsmWidgets
  use iso_c_binding
  use common_Dsm
  
  implicit none
  type(c_ptr) :: window
  type(c_ptr) :: box1, table
  type(c_ptr) :: button1, button2, button3, button4,button5
  type(c_ptr) :: label1, label2, label3, label4, label5
  type(c_ptr) :: entry1, entry2, entry3, entry4, entry5
  type(c_ptr) :: file_selector
  type(c_ptr) :: drawing_area
  type(c_ptr) :: dialog

! Data transmitted to the widgets                                      !     
! e1, e2, e3 : pointers on Entry widgets                               !
! fin : pointer of file selector for input data                        !
! pStruct : pointer on the structure data                              !
! ReadOk : data flag for reading procedure                             !
!          1 the file has been read                                    !
!          0 the file hasn't been read yet                             !
  type Wdgts
     type(c_ptr)::e1,e2,e3,e4,e5
     type(c_ptr)::fin
     type(c_ptr)::d1
     type(structure),pointer::pStruct
     integer::ReadOk
  end type Wdgts
end module dsmWidgets


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! GTK Interface functions                                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module handlers
  use gtk_hl
  use gtk_draw_hl
  use gdk_pixbuf_hl
  use gtk_sup
  use iso_c_binding
  use g, only: g_usleep
  use gtk, only: gtk_about_dialog_new,gtk_about_dialog_set_authors, &
  & gtk_about_dialog_set_comments,gtk_about_dialog_set_license, &
  & gtk_about_dialog_set_program_name,gtk_about_dialog_set_website,&
  & gtk_button_new,gtk_button_new_with_label,gtk_container_add,&
  & gtk_container_set_border_width,gtk_dialog_run,gtk_drawing_area_new,&
  & gtk_entry_get_text, gtk_entry_new, gtk_file_chooser_button_new,&
  & gtk_file_chooser_get_file,gtk_label_new, gtk_main,gtk_main_quit,&
  & gtk_progress_bar_new,gtk_progress_bar_pulse,&
  & gtk_progress_bar_set_fraction,gtk_progress_bar_set_text,&
  & gtk_scrolled_window_new,gtk_table_attach,gtk_table_attach_defaults,&
  & gtk_table_new,gtk_text_buffer_set_text,gtk_text_view_get_buffer,&
  & gtk_text_view_new,gtk_widget_destroy,gtk_widget_get_window,&
  & gtk_widget_show,gtk_widget_show_all,gtk_window_new,&
  & gtk_window_set_default,gtk_window_set_default_size,&
  & gtk_window_set_title,g_signal_connect,gtk_init,FALSE,TRUE,&
  & c_null_char,GDK_COLORSPACE_RGB,GDK_COLORSPACE_RGB,&
  & GTK_WINDOW_TOPLEVEL,c_null_ptr
  use cairo, only: cairo_create,cairo_curve_to, cairo_destroy,&
  & cairo_line_to,cairo_move_to,cairo_paint,cairo_set_line_width,&
  & cairo_set_source,cairo_set_source_rgb, cairo_stroke,&
  & cairo_image_surface_get_width, cairo_rectangle, cairo_show_text
  use gdk, only: gdk_cairo_set_source_pixbuf, gdk_cairo_create 
  use gdk_pixbuf, only: gdk_pixbuf_get_has_alpha,&
  & gdk_pixbuf_get_n_channels,gdk_pixbuf_get_pixels,&
  & gdk_pixbuf_get_rowstride,gdk_pixbuf_new
  use gtk_os_dependent, only: gtk_file_chooser_get_filename,&
  & gdk_pixbuf_new_from_file
  use dsmWidgets

  implicit none

  contains

! GtkWidget events :                                                   !
  function delete_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata
    ret = FALSE
  end function delete_event

  function expose_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int, c_char
    implicit none
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata
    type(c_ptr) :: cc, pixbuf, image
    character(kind=c_char), dimension(:), pointer :: pixel
    integer :: j
    integer(kind=c_int) :: i, nch, rowstride, width, height
    integer :: x, y

! Supmeca logo on left pixbuf widget                                   !    
    cc = gdk_cairo_create (gtk_widget_get_window(widget))
    image=hl_gdk_pixbuf_new_file("/home/tanguyb/DynamicStiffness/release/&
   &logo-supmeca.png")
    call gdk_cairo_set_source_pixbuf(cc,image,0d0,0d0)
    call cairo_paint(cc)

! Right RGB pixbuf unused widget                                              !    
    width = 700
    height =195
    pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8_c_int,width,&
   &height)    
    call c_f_pointer(gdk_pixbuf_get_pixels(pixbuf), pixel, (/0/))
    nch = gdk_pixbuf_get_n_channels(pixbuf)
    rowstride = gdk_pixbuf_get_rowstride(pixbuf)
!   Background color is blue                                           !
    do i=1, width*height*nch, nch
      pixel(i)=char(0)      ! Red
      pixel(i+1)=char(0)    ! Green
      pixel(i+2)=char(255)  ! Blue
      pixel(i+3)=char(20)  ! Opacity (Alpha channel)
    end do
! Location on right                                                    !    
    call gdk_cairo_set_source_pixbuf(cc,pixbuf, 260d0, 0d0)
    call cairo_paint(cc)
    call cairo_destroy(cc)
    ret = FALSE
  end function expose_event

! What happened when the main window is closed                         !
  subroutine destroy (widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    print *, "Goodbye"
    call gtk_main_quit()
  end subroutine destroy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! GtkButton callbacks                                                  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                                     
! What happens when the read button is clicked                        !
  function readbutton (widget, gdata ) result(ret)  bind(c)
    use common_Dsm
    use iso_c_binding, only: c_ptr, c_char, c_int
    
    implicit none
    type(c_ptr), value :: widget, gdata
    integer(c_int)    :: ret
    
    character(kind=c_char), dimension(:), pointer :: textptr
    type(c_ptr)::c_filename
    type(Wdgts), pointer::val
       
    character(len=80)::filename
    double precision::xmin,xmax,ymin,ymax
    double precision, dimension(2)::xy
    integer::i
    

	call c_f_pointer(gdata, val)

! Data file is read if selected                                        !
    c_filename=gtk_file_chooser_get_filename (val%fin)
    call c_f_pointer(c_filename,textptr,(/0/))
    if (associated(textptr).EQV..TRUE.) then
       call cstring2fstring(textptr,filename)
       print *, "Data file is :"
       print *, TRIM(filename)
       print *, "Reading data file ..."
       if (filename(LEN_TRIM(filename)-2:LEN_TRIM(filename)).eq.'dat')&
       &then
		  call readdatafile(filename,nmax,emax,mmax,smax,&
		  &val%pStruct%cat,val%pStruct%nodes,val%pStruct%nn,&
		  &val%pStruct%elems,val%pStruct%ne,val%pStruct%mates,&
		  &val%pStruct%nm,val%pStruct%sects,val%pStruct%ns)
	   elseif (filename(LEN_TRIM(filename)-2:LEN_TRIM(filename)).eq.&
	   'inp') then
		  call readinputfile(filename,nmax,emax,mmax,smax,&
		  &val%pStruct%cat,val%pStruct%nodes,val%pStruct%nn,&
		  &val%pStruct%elems,val%pStruct%ne,val%pStruct%mates,&
		  &val%pStruct%nm,val%pStruct%sects,val%pStruct%ns)
		  
		  call writedatafile(filename,nmax,emax,mmax,smax,&
		  &val%pStruct%cat,val%pStruct%nodes,val%pStruct%nn,&
		  &val%pStruct%elems,val%pStruct%ne,val%pStruct%mates,&
		  &val%pStruct%nm,val%pStruct%sects,val%pStruct%ns)
		  print *, 'File ', trim(filename), ' created'
	   endif
    else
      print*,"Data file is not selected"
      readOk=0
      goto 10
    end if

! Characteristics of the structure are displayed on the terminal       !
    PRINT*
    WRITE(*,*) 'Type of structure : ',val%pStruct%cat
    WRITE(*,*) 'Number of nodes : ', val%pStruct%nn
    WRITE(*,*) 'Number of elements : ',val%pStruct%ne
    WRITE(*,*) 'Number of materials : ',val%pStruct%nm
    WRITE(*,*) 'Number of sections : ',val%pStruct%ns
    PRINT*
! Timoshenko's parameters are added : 04/2021						   !
    WRITE(*,'(3A3,7A9,A7)') 'NB','N1','N2','DENS','E','TgD','Nu','S',&
   &                        'IZ','kY','THEORY'
    DO i=1,val%pStruct%NE
       WRITE(*,'(3I3,7D9.2,I7)') i,val%pStruct%elems(i,1),&
      &val%pStruct%elems(i,2),&
      &val%pStruct%mates(val%pStruct%elems(i,3),1),&
      &val%pStruct%mates(val%pStruct%elems(i,3),2),&
      &val%pStruct%mates(val%pStruct%elems(i,3),3),&
      &val%pStruct%mates(val%pStruct%elems(i,3),4),&
      &val%pStruct%sects(val%pStruct%elems(i,4),1),&
      &val%pStruct%sects(val%pStruct%elems(i,4),2),&
      &val%pStruct%sects(val%pStruct%elems(i,4),3),&
      &val%pStruct%elems(i,5)
    ENDDO
    
! Min and max coordinates are calculated and stored for display        !
     xy=minval(val%pStruct%nodes,dim=1) 
     val%pStruct%xmin=xy(1)
     val%pStruct%ymin=xy(2)
     xy=maxval(val%pStruct%nodes,dim=1)
     val%pStruct%xmax=xy(1)
     val%pStruct%ymax=xy(2)

! The structure is displayed in the graphical area                     !                   
    call draw_2Dframe(draw,gdata)
    readOk=1
10  ret = FALSE
  end function readbutton
  
! What happens when the proceed button is clicked : the main frequency ! 
! loop for response computation                                        !   
  function proceedbutton (widget, gdata ) result(ret)  bind(c)
     use iso_c_binding, only: c_ptr
     
     implicit none
     integer(c_int)    :: ret
     type(c_ptr), value :: widget, gdata
	 	 
	 type(Wdgts), pointer::val
     character(kind=c_char), dimension(:), pointer :: textptr
     type(c_ptr) :: ctext
     character(len=100)::ftext
     character(len=80)::filename
     integer::n,nf,info,idof,odof,i,j
     real(kind=8)::w,f,pi,f1,f2,fstep
     integer,dimension(nmax)::ipiv
     complex(kind=8),dimension(nmax*3,nmax*3)::kwst
     complex(kind=8),dimension(3*nmax)::b,work
     
     call c_f_pointer(gdata, val) 
     if(readOk.EQ.1) then
       call c_f_pointer(gtk_file_chooser_get_filename (val%fin),&
      &textptr, (/0/))
       call cstring2fstring(textptr,filename)

! The first, the last and the number of processed frequencies are      !
! obtained in C-string format from gtk widgets and changed into        !
! double precision numerics                                            !       
        ctext=gtk_entry_get_text(val%e1)       
	    call c_f_string(ctext,ftext)
	    if (ftext.EQ."") then
			write(*,*) "Missing first frequency"
			goto 10
		end if
	    read(ftext,*) f1
	    ctext=gtk_entry_get_text(val%e2)
   	    call c_f_string(ctext,ftext)
   	    if (ftext.EQ."") then
			write(*,*) "Missing last frequency"
			goto 10
		end if
	    read(ftext,*) f2
	    ctext=gtk_entry_get_text(val%e3)
	    call c_f_string(ctext,ftext)
	    if (ftext.EQ."") then
			write(*,*) "Missing number of frequencies"
			goto 10
		end if
   	    read(ftext,*) nf

! Input and output DOF are obtained in C-string format from gtk widgets!
! and changed into integer numerics                                    !   
   	    ctext=gtk_entry_get_text(val%e4)
   	    call c_f_string(ctext,ftext)
   	    if (ftext.EQ."") then
			write(*,*) "Missing input DOF"
			goto 10
		end if
	    read(ftext,*) idof
   	    ctext=gtk_entry_get_text(val%e5)
   	    call c_f_string(ctext,ftext)
   	    if (ftext.EQ."") then
			write(*,*) "Missing output DOF"
			goto 10
		end if
	    read(ftext,*) odof
  
   	    
! Result file is created and opened                                    !
        OPEN(10,FILE=filename(1:INDEX(filename,'.'))//'res')
        WRITE(10,*) nf
        pi=ACOS(-1.0)	
        fstep=(f2-f1)/(nf-1)
        F=F1
        
! The main frequency loop                                              !
        DO n=1,nf
           w=2*pi*f
! The dynamic stiffness matrix is re-calculated for each frequency     !
           kwst(1:3*val%pStruct%nn,1:3*val%pStruct%nn)=dcmplx(0.D0,0.D0)
           CALL DYNAMICSTIFFNESS2D(W,val%pStruct%nodes,&
          &val%pStruct%elems,val%pStruct%mates,val%pStruct%sects,nmax,&
          &emax,mmax,smax,val%pStruct%ne,3*nmax,kwst)

! A unit force is stored in second member b                            ! 
           b(1:3*val%pStruct%nn)=dcmplx(0.0,0.0)    
           b(idof)=dcmplx(1.0,0.0)

! The algebraic system is solved with Lapack library                   !
           CALL ZSYSV('U',3*val%pStruct%nn,1,kwst,3*nmax,ipiv,b,3*nmax,&
          &work,3*nmax,info)

! Solution stored in second member b is displayed and saved in result  !
! file                                                                 ! 
           WRITE(*,*) f,cdabs(b(odof))          
           WRITE(10,*) f,cdabs(b(odof))
	       F=F+FSTEP
        ENDDO
        CLOSE(10)
        WRITE(*,*) 'Result file is '//filename(1:INDEX(filename,'.'))&
       &//'res'
     else
        print*, "Data file is not read"
     end if
10   ret=FALSE
  end function proceedbutton
  
! What happens when the structural response button is clicked          !   
  function structuralResponse(widget,gdata) result(ret) bind(c)
     use iso_c_binding, only: c_ptr
     
     implicit none
     integer(c_int)    :: ret
     type(c_ptr), value :: widget, gdata
	 type(Wdgts), pointer::val
     character(kind=c_char), dimension(:), pointer :: textptr
     type(c_ptr)::c_filename
     type(c_ptr) :: ctext
     character(len=80)::ftext,filename
     integer::nfreq,i,nfreqmax
     PARAMETER (nfreqmax=10000)
     real(kind=8)::resp(nfreqmax,2)
  
     call c_f_pointer(gdata, val) 

! Result file is read if exists                                        !           
    c_filename=gtk_file_chooser_get_filename (val%fin)
    call c_f_pointer(c_filename,textptr,(/0/))
    if (associated(textptr).EQV..TRUE.) then
       call cstring2fstring(textptr,filename)
       print*, "Result file is :"
       print*, filename(1:index(filename,'.'))//'res'
       ctext=gtk_entry_get_text(val%e3)
	   call c_f_string(ctext,ftext)
       print*, "Reading ",trim(ftext)," frequencies"

! The response is read                                                 !       
       open(10,FILE=filename(1:index(filename,'.'))//'res')
       read(10,*) nfreq
       do i=1,nfreq
           read(10,*,end=10,err=10) resp(i,1),resp(i,2)
           resp(i,2)=20*dlog10(resp(i,2))
       enddo
       close(10)

! The response is drawn                                                !
       call draw_curve(draw,gdata,NFREQ,NFREQMAX,RESP)
    else
       write(*,*) "Data file is not selected"
    end if
10  continue
  end function structuralResponse
  
! What happens when clear graphic button is clicked                    !
  function clearGraphic(widget,gdata) result(ret) bind(c)
     use iso_c_binding, only: c_ptr
     integer(c_int)    :: ret
     type(c_ptr), value :: widget, gdata
     type(c_ptr)::cc
     
! Cairo context is created on draw widget                              !     
     cc = hl_gtk_drawing_area_cairo_new(draw)

! Black is created
     call cairo_set_source_rgb(cc,0._c_double,0._c_double,0._c_double)

! Black rectangle is createdvand painted                               !
     call cairo_rectangle(cc, 0._c_double, 0._c_double,&
    & real(width, c_double), real(height, c_double))
      call cairo_paint (cc);
      call cairo_stroke(cc)
      call gtk_widget_queue_draw(draw)
      call hl_gtk_drawing_area_cairo_destroy(cc)
  end function clearGraphic

! What happens when about button is clicked                            !  
  function aboutbutton (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr
    
    implicit none
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    integer(c_int) :: response_id
    
    dialog = gtk_about_dialog_new()
    call gtk_about_dialog_set_program_name(dialog, "Dynamic Stiffness  &
   &1.0"//c_null_char)
    call gtk_about_dialog_set_license(dialog, "GNU GPL 3"//c_null_char)
    call gtk_about_dialog_set_comments(dialog, "This program is a&
   & Fortran implementation of the dynamic stiffness method. Version &
   &1.0 is limited to planar straight beam assemblies and&
   & post-processing operations are limited to the computation of &
   &harmonic responses for free boundary conditions."//c_new_line//&
   &"Both textual and graphical versions exist. The actual graphical&
   & version dsmGtk is based on the free graphical library&
   & GTK-fortran."//c_new_line//"This software is developped at Supmeca&
   & by J.B. Casimir"//c_null_char)
    call gtk_about_dialog_set_website(dialog, &
   &"https://github.com/jerryd/gtk-fortran/wiki"//c_null_char)
    response_id =  gtk_dialog_run(dialog)
    call gtk_widget_destroy(dialog)
    ret = FALSE
  end function aboutbutton

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! File-Selector Callback                                               ! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                     
  function file_changed (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_char

    implicit none
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    ret = FALSE
  end function file_changed

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! globals width and height are re-calculated if window's dimensions are!
! changed                                                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine resize_area(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    type(Wdgts),pointer::val
    type(gtkallocation), target:: alloc
    
    call gtk_widget_get_allocation(draw,c_loc(alloc))
    call hl_gtk_drawing_area_resize(draw)
    width=alloc%width
    height=alloc%height
  end subroutine resize_area

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! The following functions are not widget callbacks                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! c-string to f-string function conversion                             !
  subroutine cstring2fstring(textptr, f_string)
    use iso_c_binding, only: c_char

    implicit none
    character(kind=c_char), dimension(:), pointer, intent(in) :: textptr
    character(len=*), intent(out) :: f_string
    integer :: i
          
    f_string=""
    i=1
    do while(textptr(i) .NE. char(0))
      f_string(i:i)=textptr(i)
      i=i+1
    end do
  end subroutine cstring2fstring
  
! global coordinates to pixels conversion                              ! 
  subroutine coords2pixs(x,y,xmin,xmax,ymin,ymax,px,py,w,h,aspectRatio)
     implicit none
     real(kind=8)::x,y,xmin,xmax,ymin,ymax,dx,dy,fx,fy,f,mx,my
     real(kind=8)::px,py,aspectRatio
     integer w,h
     
     dx=xmax-xmin
     dy=ymax-ymin
     fx=(w-40)/dx
     fy=(h-40)/dy
     mx=(xmin+xmax)/2
     my=(ymin+ymax)/2
     if (fx.LT.fy) then
       f=fx
     else
       f=fy
     end if
     if(aspectRatio.EQ.1.D0) then
        px=w/2+(x-mx)*f
        py=h/2-(y-my)*f
     else
       px=w/2+(x-mx)*fx
       py=h/2+(my-y)*fy
     endif
  end subroutine coords2pixs

!  Drawing structure subroutine                                        !
   subroutine draw_2Dframe(area,gdata)
    implicit none
    type(c_ptr),value :: area,gdata
    type(Wdgts),pointer::val
    type(c_ptr) :: cc 
    real(kind=8)::x1,y1,x2,y2,xmin,xmax,ymin,ymax,px1,py1,px2,py2
    integer i
       
    call c_f_pointer(gdata, val)

! Real dimensions of the structure                                     !
    xmin=val%pStruct%xmin
    ymin=val%pStruct%ymin
    xmax=val%pStruct%xmax
    ymax=val%pStruct%ymax
   
! Get a cairo context from the drawing area.
    cc = hl_gtk_drawing_area_cairo_new(area)
    
! White color is selected                                              !
    call cairo_set_source_rgb(cc,1._c_double,1._c_double,1._c_double)
    call cairo_set_line_width(cc, 1d0)
    
! Each beam is drawn                                                   !
    do i=1,val%pStruct%ne
       x1=val%pStruct%nodes(val%pStruct%elems(i,1),1)
       y1=val%pStruct%nodes(val%pStruct%elems(i,1),2)
       call coords2pixs(x1,y1,xmin,xmax,ymin,ymax,px1,py1,width,height,&
      &1.D0)
       x2=val%pStruct%nodes(val%pStruct%elems(i,2),1)
       y2=val%pStruct%nodes(val%pStruct%elems(i,2),2)
       call coords2pixs(x2,y2,xmin,xmax,ymin,ymax,px2,py2,width,height,&
      &1.D0)
       call cairo_move_to(cc, px1,py1)  
       call cairo_line_to(cc, px2,py2)
       call cairo_stroke(cc) 
    enddo
    call gtk_widget_queue_draw(area)
    call hl_gtk_drawing_area_cairo_destroy(cc)
  end subroutine draw_2Dframe
  
! Drawing curve subroutine                                             ! 
    subroutine draw_curve(area,gdata,nfreq,nfreqmax,curve) 
    implicit none
    type(c_ptr),value :: area,gdata
    integer :: nfreqmax,nfreq
    integer :: I
    double precision :: curve(nfreqmax,2)
    character(len=12)::f_text
    double precision::xmin,xmax,ymin,ymax,xy(2),x,y,px,py,dx
    type (c_ptr) :: cc,c_text

! Dimensions of the curve are calculated                               !
    xy=minval(curve,dim=1) 
    xmin=xy(1)
    ymin=xy(2)
    xy=maxval(curve,dim=1)
    xmax=xy(1)
    ymax=xy(2)
    
! Get a cairo context from the drawing area.
    cc = hl_gtk_drawing_area_cairo_new(area)
    
! White color is selected                                              !
    call cairo_set_source_rgb(cc,1.0_c_double,1.0_c_double,1.0_c_double)
    call cairo_set_line_width(cc, 1d0)
    
! First point of the curve
    x=curve(1,1)
    y=curve(1,2)
    call coords2pixs(x,y,xmin,xmax,ymin,ymax,px,py,width,height,2.D0)
    call cairo_move_to(cc,px,py)
    
! Each point of the curve is plotted                                   !
    do i=2,nfreq
       x=curve(i,1)
       y=curve(i,2)
       call coords2pixs(x,y,xmin,xmax,ymin,ymax,px,py,width,height,2.D0)
       call cairo_line_to(cc,px,py)
       call cairo_stroke(cc)
       call cairo_move_to(cc,px,py)
    enddo
    
! Axes are drawn                                                       !
    call cairo_move_to(cc,20.D0,real(height-20.D0,8))
    call cairo_line_to(cc,real(width-20,8),real(height-20.D0,8))
    call cairo_stroke(cc)
    call cairo_move_to(cc,20.D0,real(height-20.D0,8))
    call cairo_line_to(cc,20.D0,20.D00)
    call cairo_stroke(cc)
    dx=(curve(nfreq,1)-curve(1,1))/10
    
! Graduation segments are drawn
    do i=1,10
      x=curve(i,1)+i*dx
      write(f_text,'(D8.2)') x
      call cairo_move_to(cc,20.D0+i*real(width-40,8)/10,&
     &real(height-20.D0,8))
      call cairo_line_to(cc,20.D0+i*real(width-40,8)/10,&
     &real(height-20.D0,8)-10)
      call cairo_stroke(cc)
      call cairo_move_to(cc,20.D0+(i-0.5)*real(width-40,8)/10,&
     &real(height-10.D0,8))
      call cairo_show_text(cc,f_text)
      call cairo_move_to(cc,real(width,8)/2,real(height-2.D0,8))
      call cairo_show_text(cc,"Frequency (Hz)"//c_null_char)
      call cairo_move_to(cc,20.D0,20.D0+(i-1)*real(height-40.D0,8)/10)
      call cairo_line_to(cc,30.D0,20.D0+(i-1)*real(height-40.D0,8)/10)
      call cairo_stroke(cc)
    enddo
    
! Graduation texts are displayed                                       !
    write(f_text,'(D8.2)') ymin
    call cairo_move_to(cc,20.D0,real(height-25.D0,8))
    call cairo_show_text(cc,f_text)
    write(f_text,'(D8.2)') ymax
    call cairo_move_to(cc,20.D0,20.D0)
    call cairo_show_text(cc,f_text)
    call cairo_move_to(cc,1.D0,10.D0)
    call cairo_show_text(cc,"(dB)"//c_null_char)
    call gtk_widget_queue_draw(area)
    call hl_gtk_drawing_area_cairo_destroy(cc)
  end subroutine draw_curve

end module handlers

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! The main program                                                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program dsm
  use handlers
  use dsmWidgets
  use common_dsm
 
  implicit none
  type(Wdgts),target:: eW
  type(Structure),target:: Frame
  type(Structure),pointer::val  
 
  readOk=0
  Frame%nn=0
  eW%pStruct=>Frame
  call gtk_init ()
  width=600
  height=600
 
! Widgets
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  box1=hl_gtk_box_new()
  draw = hl_gtk_drawing_area_new(size=(/width, height/), &
 & has_alpha = FALSE,&
 & size_allocate=c_funloc(resize_area),data_size_allocate=c_loc(eW))
  table = gtk_table_new (5_c_int, 4_c_int, TRUE)
  button1 = gtk_button_new_with_label("Read data and display"//&
 &c_null_char)
  button2 = gtk_button_new_with_label ("Proceed ..."//c_null_char)
  button3 = gtk_button_new_with_label ("Harmonic response"//c_null_char)
  button4 = gtk_button_new_with_label ("Clear graphic"//c_null_char)
  button5 = gtk_button_new_with_label ("About ..."//c_null_char)
  label1 = gtk_label_new("First frequency"//c_null_char)
  label2 = gtk_label_new("Last frequency"//c_null_char)
  label3 = gtk_label_new("Number of frequencies"//c_null_char)
  label4 = gtk_label_new("Input DOF"//c_null_char)
  label5 = gtk_label_new("Output DOF"//c_null_char)
  entry1 = gtk_entry_new()
  entry2 = gtk_entry_new()
  entry3 = gtk_entry_new()
  entry4 = gtk_entry_new()
  entry5 = gtk_entry_new()
  file_selector = gtk_file_chooser_button_new&
 &("gtk_file_chooser_button_new"//c_null_char, 0_c_int)
  drawing_area = gtk_drawing_area_new()

! Widget properties
  call gtk_window_set_title(window, "Dynamic Stiffness"//c_null_char)
  call gtk_container_set_border_width (window, 10_c_int)
  
! Widget data  
  eW%e1=entry1
  eW%e2=entry2 
  eW%e3=entry3
  eW%e4=entry4
  eW%e5=entry5
  eW%fin=file_selector
  eW%d1=drawing_area
 
   
! Widget signals
  call g_signal_connect (window, "delete-event"//c_null_char, c_funloc(delete_event))
  call g_signal_connect (window, "destroy"//c_null_char, c_funloc(destroy))
  call g_signal_connect (button1, "clicked"//c_null_char, c_funloc(readbutton),c_loc(eW))
  call g_signal_connect (button2, "clicked"//c_null_char, c_funloc(proceedbutton),c_loc(eW))
  call g_signal_connect (button3, "clicked"//c_null_char, c_funloc(structuralResponse),c_loc(eW))
  call g_signal_connect (button4, "clicked"//c_null_char, c_funloc(clearGraphic),c_loc(eW))
  call g_signal_connect (button5, "clicked"//c_null_char, c_funloc(aboutbutton),c_loc(eW))
  call g_signal_connect (file_selector, "selection-changed"//c_null_char,c_funloc(file_changed));
  call g_signal_connect (drawing_area, "expose-event"//c_null_char, c_funloc(expose_event))
  
! Widget positions
   call gtk_container_add(window,box1)
   call hl_gtk_box_pack(box1, draw)
   call gtk_container_add (box1, table)
   call gtk_table_attach_defaults(table, file_selector, 0_c_int, 1_c_int,0_c_int, 1_c_int)
   call gtk_table_attach_defaults(table, button1, 1_c_int, 2_c_int,0_c_int, 1_c_int)
   call gtk_table_attach_defaults(table, label1, 0_c_int, 1_c_int,1_c_int, 2_c_int)
   call gtk_table_attach_defaults(table, entry1, 1_c_int, 2_c_int,1_c_int, 2_c_int)
   call gtk_table_attach_defaults(table, label4, 2_c_int, 3_c_int,1_c_int, 2_c_int)
   call gtk_table_attach_defaults(table, entry4, 3_c_int, 4_c_int,1_c_int, 2_c_int) 
   call gtk_table_attach_defaults(table, label2, 0_c_int, 1_c_int,2_c_int, 3_c_int) 
   call gtk_table_attach_defaults(table, entry2, 1_c_int, 2_c_int,2_c_int, 3_c_int) 
   call gtk_table_attach_defaults(table, label5, 2_c_int, 3_c_int,2_c_int, 3_c_int)
   call gtk_table_attach_defaults(table, entry5, 3_c_int, 4_c_int,2_c_int, 3_c_int)  
   call gtk_table_attach_defaults(table, label3, 0_c_int, 1_c_int,3_c_int, 4_c_int)
   call gtk_table_attach_defaults(table, entry3, 1_c_int, 2_c_int,3_c_int, 4_c_int) 
   call gtk_table_attach_defaults(table, button2, 2_c_int, 3_c_int,3_c_int, 4_c_int)
   call gtk_table_attach_defaults(table, drawing_area, 0_c_int, 4_c_int,4_c_int, 10_c_int)
   call gtk_table_attach_defaults(table, button3, 3_c_int, 4_c_int,3_c_int, 4_c_int)  
   call gtk_table_attach_defaults(table, button4, 2_c_int, 3_c_int,0_c_int, 1_c_int)  
   call gtk_table_attach_defaults(table, button5, 3_c_int, 4_c_int,0_c_int, 1_c_int)
   call gtk_widget_show_all (window)
 
   call gtk_main ()

end program dsm 
