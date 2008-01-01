%define _plugindir %{_libdir}/gimp/2.0/plug-ins
%define _mathmapdir %{_datadir}/gimp/2.0/mathmap
%define _langdir %{_datadir}/gtksourceview-1.0/language-specs

Name:           mathmap
Version:        1.3.0
Release:        1
License:        GPL
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
Group:		Applications/Multimedia
Summary:	MathMap GIMP Plug-In and Command-Line Tool
URL:		http://www.complang.tuwien.ac.at/schani/mathmap/
Source:		%{name}_%{version}-1.tar.gz
Requires: gcc
Requires: libpng
Requires: libjpeg
Requires: giflib
Requires: gsl
Requires: gimp
BuildRequires: libpng-devel
BuildRequires: libjpeg-devel
BuildRequires: giflib-devel
BuildRequires: gsl-devel
BuildRequires: gimp-devel
BuildRequires: gimp
BuildRequires: make
BuildRequires: gtksourceview-devel
%if %{?suse_version:1}0
%if %suse_version > 1020
BuildRequires: gtksourceview18-devel
%endif
%endif
%if %{?fedora_version:1}0
%if %fedora_version == 8
BuildRequires: lynx
%endif
%endif

%description
MathMap is a GIMP plug-in which allows distortion of images specified
by mathematical formulae.  For each pixel in the generated image, an
expression is evaluated which should return a pixel value.  The
expression can either refer to a pixel in the source image or can
generate pixels completely independent of the source.

%prep
%setup

%build
%{__make}

%install
install -d $RPM_BUILD_ROOT/%{_bindir}
install -d $RPM_BUILD_ROOT/%{_plugindir}
install -d $RPM_BUILD_ROOT/%{_mathmapdir}
install -d $RPM_BUILD_ROOT/%{_langdir}
install mathmap $RPM_BUILD_ROOT/%{_bindir}/mathmap
ln -s %{_bindir}/mathmap $RPM_BUILD_ROOT/%{_plugindir}/mathmap
install new_template.c opmacros.h lispreader/pools.h $RPM_BUILD_ROOT/%{_mathmapdir}/
install generators/blender/blender_template.c generators/blender/blender_opmacros.h $RPM_BUILD_ROOT/%{_mathmapdir}/
install pixmaps/*.png $RPM_BUILD_ROOT/%{_mathmapdir}/
install mathmap.lang $RPM_BUILD_ROOT/%{_langdir}/
cp -r examples $RPM_BUILD_ROOT/%{_mathmapdir}/expressions

%clean
rm -rf "$RPM_BUILD_ROOT"

%files
%defattr(-,root,root)
%doc ANNOUNCEMENT COPYING README README.filters README.mercurial
%{_bindir}/mathmap
%{_plugindir}/mathmap
%{_langdir}/mathmap.lang
%{_mathmapdir}

%changelog
* Tue Jan 01 2008 Mark Probst <schani@complang.tuwien.ac.at> 1.3.0
- Update for version 1.3.0

* Mon Dec 03 2007 Mark Probst <schani@complang.tuwien.ac.at> 1.2.4
- openSUSE Build Service

* Fri Nov 23 2007 Mark Probst <schani@complang.tuwien.ac.at> 1.2.4
- Update for version 1.2.4

* Fri Nov 09 2007 Mark Probst <schani@complang.tuwien.ac.at> 1.2.3
- Update for version 1.2.3

* Sun Nov 04 2007 Mark Probst <schani@complang.tuwien.ac.at> 1.2.2
- Update for version 1.2.2

* Thu May 04 2007 Mark Probst <schani@complang.tuwien.ac.at> 1.2.0
- Update for version 1.2.0

* Thu Apr 12 2007 Mark Probst <schani@complang.tuwien.ac.at> 1.1.3
- First creation of spec file
