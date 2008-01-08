# Copyright 1999-2007 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header: $

inherit qt4 multilib autotools

DESCRIPTION="Open source web browser engine"
HOMEPAGE="http://www.webkit.org/"
MY_P="WebKit-r${PV}"
SRC_URI="http://nightly.webkit.org/files/trunk/src/${MY_P}.tar.bz2"

LICENSE="LGPL-2 LGPL-2.1 BSD"
SLOT="0"
KEYWORDS="~x86 ~amd64"
IUSE="debug gstreamer gtk icondb qt4 svg xslt xpath"

S="${WORKDIR}/${MY_P}"

RDEPEND=">=dev-db/sqlite-3
		xslt? ( >=dev-libs/libxslt-1.1.7 )
		qt4? ( $(qt4_min_version 4.3) )
		gtk? (
				>=x11-libs/gtk+-2.0
				dev-libs/icu
				>=net-misc/curl-7.15
				media-libs/jpeg
				media-libs/libpng
				gstreamer? ( >=media-libs/gstreamer-0.10 )
			 )"

DEPEND="${RDEPEND}
		qt4? ( $(qt4_min_version 4) )
		sys-devel/bison
		dev-util/gperf
		>=sys-devel/flex-2.5.33"

src_unpack() {
	unpack ${A}
	cd "${S}"

	if use gtk; then
		eautoreconf
		automake --add-missing
	fi
}

src_compile_autotools() {
	local myconf="$(use_enable xslt)	\
					$(use_enable xpath)	\
					$(use_enable svg) \
					$(use_enable icondb icon-database)"

	if use debug; then
		myconf="${myconf} --enable-debug"
	fi
	if use gstreamer ; then
		myconf="${myconf} --enable-video"
	fi

	econf ${myconf} || die "configure failed"
}

src_compile_qmake() {
	myconf="CONFIG-=gtk-port CONFIG+=qt-port"
	if use debug; then
		myconf="${myconf} CONFIG+=debug CONFIG-=release"
	else
		myconf="${myconf} CONFIG+=release CONFIG-=debug"
	fi

	eqmake4 WebKit.pro -recursive \
	OUTPUT_DIR=${S} \
	WEBKIT_INC_DIR=/usr/include/${PN} \
	WEBKIT_LIB_DIR=/usr/$(get_libdir) \
	${myconf} \
	|| die "eqmake4 failed"
}

src_compile() {
	ewarn "This ebuild installs directly from a nightly build."
	ewarn "The program might be unstable some times."
	einfo "If anything goes erroneous, please file a bug report at http://bugs.webkit.org."

	if use gtk; then
		src_compile_autotools
	else
		src_compile_qmake
	fi

	emake -j1 || die "emake failed"
}

src_install() {
	if use gtk; then
		einstall || die "Installation failed"
	else
		emake INSTALL_ROOT="${D}" install || die "Installation failed"
	fi
}
