# Copyright 1999-2007 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header: $

inherit autotools eutils multilib versionator toolchain-funcs subversion

ESVN_REPO_URI="http://syx.googlecode.com/svn/trunk"

MY_P="syx-${PV}"
S="${WORKDIR}/${MY_P}"
DESCRIPTION="Smalltalk YX is an open source implementation of the Smalltalk-80 programming language."
HOMEPAGE="http://syx.googlecode.com"
LICENSE="MIT"
SLOT="0"
KEYWORDS="amd64 ~arm ~ia64 ppc ppc64 sparc sparc-fbsd x86 x86-fbsd"
IUSE="gmp readline gtk debug profile iprofile"

DEPEND="!build? (
		gmp? ( >=dev-libs/gmp-4.2 )
		readline? ( >=sys-libs/readline-4.1 )
                gtk? ( >=x11-libs/gtk+-2.12 )
	)"

