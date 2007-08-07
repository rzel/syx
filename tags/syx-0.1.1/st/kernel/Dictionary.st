"
Copyright (c) 2007 Luca Bruno

This file is part of Smalltalk YX.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the 'Software'), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
"

!Dictionary class methodsFor: 'instance creation'!

new: size
    ^self basicNew
	initializeTable: size;
	yourself
!

new
    ^self new: 30
! !

!Dictionary methodsFor: 'initialize-release'!

initializeTable: size
    hashTable := Array new: size + 3
! !

!Dictionary methodsFor: 'private'!

indexFor: aHash
    <primitive: 'Dictionary_pos'>
	self primitiveFailed
! !

!Dictionary methodsFor: 'accessing'!

removeKey: aKey
    1 to: hashTable size by: 2 do: [ :i |
	(hashTable at: i) = aKey
	    ifTrue: [ hashTable at: i put: nil. ^hashTable at: i + 1 put: nil ] ]
!

indexOf: aKey
    | h2 pos size |
    size := hashTable size.
    pos := self indexFor: aKey hash.
    h2 := pos / 4.

    (h2 between: 0 and: 1)
	ifTrue: [ h2 := 2 ].
    (h2 \\ 2) isZero
	ifFalse: [ h2 := h2 - 1 ].

    1 to: hashTable size do: [ :i |
	| key |
	key := hashTable at: pos + 1.
	key = aKey | key isNil
	    ifTrue: [ ^pos + 1 ].
	pos := pos + h2.
	pos >= (size - 1)
	    ifTrue: [ pos := pos - (size - 1) ] ]
!

at: aKey
    | index key |
    index := self indexOf: aKey.
    (hashTable at: index) isNil
	ifTrue: [ NotFound signal: 'key not found' ].
    ^hashTable at: index + 1
!

at: aKey put: aValue
    | index |
    index := self indexOf: aKey.
    hashTable
	at: index put: aKey;
	at: index + 1 put: aValue.
    ^aValue
! !