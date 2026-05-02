Name:           cave
Version:        0.1.0
Release:        1%{?dist}
Summary:        Cave

License:        MIT
URL:            https://github.com/OWNER/cave
Source0:        cave-%{version}.tar.gz

%global debug_package %{nil}
%global _build_id_links none
%global __strip /bin/true
%global __brp_strip %{nil}
%global __brp_strip_comment_note %{nil}
%global __brp_strip_static_archive %{nil}

BuildRequires:  sbcl
BuildRequires:  libfixposix-devel
BuildRequires:  gcc
BuildRequires:  make

%description
Cave - a Common Lisp application.

%prep
%autosetup

%build
make

%install
install -D -m 0755 cave %{buildroot}%{_bindir}/cave
install -D -m 0644 cave-sbom.spdx.json %{buildroot}%{_datadir}/sbom/cave-%{version}.spdx.json

%files
%license LICENSE
%doc README.md
%{_bindir}/cave
%{_datadir}/sbom/cave-%{version}.spdx.json

%changelog
