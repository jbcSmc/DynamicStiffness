all:release/dsmGtk
GFC=gfortran
LDFLAGS=-L./lib -llapack -lm
GTK_VERSION=`pkg-config --cflags --libs gtk+-2.0 plplotd-f95`

SRC=ReadDataFile.for\
    DynamicStiffness2D.for\
    GlobalStraightPlanarBeam.for\
    StraightPlanarBeam.for\
    PlanarAssembly.for\
    Traction.for\
    XYBending.for\
    XYRayleighBending.for\
    Complex16Hyperbolic.for
_OBJ=$(subst .for,.o,$(SRC))
OBJ=$(patsubst %,release/%,$(_OBJ))

_GTK_HL_OBJ=gtk-hl-misc.o\
            gtk-hl-accelerator.o\
            gtk-hl-button.o\
            gtk-hl-combobox.o\
            gtk-hl-container.o\
            gtk-hl-entry.o\
            gtk-hl-menu.o\
            gtk-hl-progress.o\
            gtk-hl-spin-slider.o\
            gtk-hl-tree.o\
            gtk-hl-chooser.o\
            gtk-hl-dialog.o\
            gtk-hl-infobar.o\
            gtk-hl-assistant.o\
            gdk-pixbuf-hl.o\
	        gtk.o\
	        gtk-sup.o\
            gtk-hl.o\
            unixonly-auto.o\
            gtk-draw-hl.o
GTK_HL_OBJ=$(patsubst %,lib/%,$(_GTK_HL_OBJ))           

_GTK_SRC=unixonly.f90\
         gdk-auto.f90\
         glib-auto.f90\
         gtk.f90\
         atk-auto.f90\
         cairo-auto.f90\
         gdk-pixbuf-auto.f90\
         pango-auto.f90\
         gtk-sup.f90\
         gtk-hl-misc.f90\
         gtk-hl-accelerator.f90\
         gtk-hl-button.f90\
         gtk-hl-combobox.f90\
         gtk-hl-container.f90\
         gtk-hl-entry.f90\
         gtk-hl-infobar.f90\
         gtk-hl-assistant.f90\
         gtk-hl-menu.f90\
         gtk-hl-progress.f90\
         gtk-hl-spin-slider.f90\
         gtk-hl-tree.f90\
         gtk-hl-chooser.f90\
         gtk-hl-dialog.f90\
         gtk-hl.f90\
         gdkevents-auto2.f90\
         gtk-draw-hl.f90\
         gdk-pixbuf-hl.f90

GTK_SRC=$(patsubst %,srcGtk/%,$(_GTK_SRC))

release/dsm:$(OBJ) release/dsm.o
	$(GFC) -o release/dsm $^ $(LDFLAGS)
	 
release/dsmGtk:$(OBJ) $(GTK_HL_OBJ) release/dsmGtk.o
	$(GFC) -o release/dsmGtk $^ $(LDFLAGS) $(GTK_VERSION) 

release/dsm.o:srcDsm/dsm.for
	$(GFC) -c srcDsm/dsm.for -o release/dsm.o
	
release/dsmGtk.o:srcDsm/dsmGtk.f90
	$(GFC) -c srcDsm/dsmGtk.f90 -o release/dsmGtk.o $(GTK_VERSION) -I srcGtk
	
release/%.o:srcDsm/%.for
	$(GFC) -c -o $@ $< $(GTK_VERSION)
	
lib/%.o:$(GTK_SRC) lib/%.mod
	$(GFC) -c -o $@ $< $(GTK_VERSION)

