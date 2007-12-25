# Copyright 1999-2007 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header: $

inherit autotools eutils subversion flag-o-matic

ESVN_REPO_URI="http://syx.googlecode.com/svn/trunk"
ESVN_PROJECT="syx-svn"
ESVN_BOOTSTRAP="autogen.sh"

MY_P="syx-${PV}"
S="${WORKDIR}/${MY_P}"
DESCRIPTION="Smalltalk YX is an open source implementation of the Smalltalk-80 programming language."
HOMEPAGE="http://syx.googlecode.com"
LICENSE="MIT"
SLOT="0"
KEYWORDS="amd64 ~arm ~ia64 ppc ppc64 sparc sparc-fbsd x86 x86-fbsd"
IUSE="gmp readline gtk debug profile iprofile"

RDEPEND="!build? (
		gmp? ( dev-libs/gmp )
		readline? ( sys-libs/readline )
                gtk? ( >=x11-libs/gtk+-2.12 )
	 )"

PROVIDE="virtual/syx"

src_compile() {
    local myconf="$(use_enable gtk) \
                  $(use_enable readline) \
                  $(use_with gmp)"

    use debug && myconf="${myconf} --enable-debug=normal"

    econf ${myconf} || die "configure failed"

    emake || die "compile failed"
}

src_test() {
    make check || die
}

src_install() {
    make DESTDIR="${D}" install || die "Installation failed"
}