# $Id$
%define _plugindir %{_libdir}/gimp/2.0/plug-ins
%define _mathmapdir %{_datadir}/gimp/2.0/mathmap

Summary: MathMap GIMP Plug-In and Command-Line Tool
Name: mathmap
Version: 1.2.2
Release: 1
License: GNU General Public License
Group: Applications/Multimedia
URL: http://www.complang.tuwien.ac.at/schani/%{name}/
Source: http://www.complang.tuwien.ac.at/schani/%{name}/%{name}-%{version}.tar.gz
Buildroot: %{_tmppath}/%{name}-%{version}-root
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

%description
MathMap is a GIMP plug-in which allows distortion of images specified
by mathematical formulae.  For each pixel in the generated image, an
expression is evaluated which should return a pixel value.  The
expression can either refer to a pixel in the source image or can
generate pixels completely independent of the source.

%prep
%setup
rm -rf $RPM_BUILD_ROOT

%build
%{__make}

%install
install -d $RPM_BUILD_ROOT/%{_bindir}
install -d $RPM_BUILD_ROOT/%{_plugindir}
install -d $RPM_BUILD_ROOT/%{_mathmapdir}
install mathmap $RPM_BUILD_ROOT/%{_bindir}/mathmap
ln -s %{_bindir}/mathmap $RPM_BUILD_ROOT/%{_plugindir}/mathmap
install new_template.c opmacros.h $RPM_BUILD_ROOT/%{_mathmapdir}/
cp -r examples $RPM_BUILD_ROOT/%{_mathmapdir}/expressions

%clean
rm -rf %{buildroot}

%files
%defattr(-, root, root)
%doc ANNOUNCEMENT COPYING README
%{_bindir}/mathmap
%{_plugindir}/mathmap
%{_mathmapdir}

%changelog
* Sun Nov 04 2007 Mark Probst <schani@complang.tuwien.ac.at> 1.2.2
- Update for version 1.2.2

* Thu May 04 2007 Mark Probst <schani@complang.tuwien.ac.at> 1.2.0
- Update for version 1.2.0

* Thu Apr 12 2007 Mark Probst <schani@complang.tuwien.ac.at> 1.1.3
- First creation of spec file
