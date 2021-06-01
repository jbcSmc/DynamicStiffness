all:release/dsmGtk release/dsm
GFC=gfortran
LDFLAGS=-L./lib -llapack -lm
GTK_VERSION=`pkg-config --cflags --libs gtk+-2.0`

_SRC=ReadDataFile.for\
	ReadInputFile.for\
	WriteDataFile.for\
    DynamicStiffness2D.for\
    GlobalStraightPlanarBeam.for\
    StraightPlanarBeam.for\
    Traction.for\
    PlanarAssembly.for\
    XYBending.for\
    XYRayleighBending.for\
    XYTimoshenkoBending.for\
    Complex16Hyperbolic.for
SRC=$(patsubst %,srcGtk/%,$(_SRC))
_OBJ=$(subst .for,.o,$(_SRC))
OBJ=$(patsubst %,lib/%,$(_OBJ))

_GTK_SRC=unixonly-auto.f90\
         gdk-auto.f90\
         glib-auto.f90\
         gtk.f90\
         cairo-auto.f90\
         gdk-pixbuf-auto.f90\
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
         gtk-draw-hl.f90\
         gdk-pixbuf-hl.f90

GTK_SRC=$(patsubst %,srcGtk/%,$(_GTK_SRC))
_GTK_HL_OBJ=$(subst .f90,.o,$(_GTK_SRC))
GTK_HL_OBJ=$(patsubst %,lib/%,$(_GTK_HL_OBJ))           

%.o:%.mod

release/dsm:$(OBJ) lib/dsm.o
	$(GFC) -o release/dsm $^ $(LDFLAGS)
	 
release/dsmGtk:$(OBJ) $(GTK_HL_OBJ) lib/dsmGtk.o
	$(GFC) -o release/dsmGtk $^ $(LDFLAGS) $(GTK_VERSION) 

lib/dsm.o:srcDsm/dsm.for
	$(GFC) -c srcDsm/dsm.for -o lib/dsm.o
	
lib/dsmGtk.o:srcDsm/dsmGtk.f90
	$(GFC) -c srcDsm/dsmGtk.f90 -o lib/dsmGtk.o $(GTK_VERSION)
	
lib/%.o:srcDsm/%.for
	$(GFC) -c -o $@ $< $(GTK_VERSION)

lib/%.o:srcGtk/%.f90
	$(GFC) -c -o $@ $< $(GTK_VERSION)

clean:
	rm release/dsm
	rm release/dsmGtk
	rm lib/*
	rm *.mod
