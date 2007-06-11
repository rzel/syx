#include <assert.h>
#include <unistd.h>
#include <glib.h>
#include <stdio.h>
#include "syx-object.h"
#include "syx-memory.h"
#include "syx-scheduler.h"

static syx_varsize _syx_memory_size;
SyxObject *syx_memory;
static SyxOop *_syx_freed_memory;
static syx_varsize _syx_memory_top = 0;
static syx_varsize _syx_freed_memory_top = 0;

static syx_bool _syx_memory_initialized = FALSE;

// Holds temporary objects that must not be freed during a transaction
static SyxOop _syx_memory_gc_trans[256];
static syx_int8 _syx_memory_gc_trans_top = 0;
static syx_bool _syx_memory_gc_trans_running = FALSE;

/*! \page syx_memory Syx Memory
    
    The objects are inserted into a whole space allocated in the heap (syx-memory.c):

    \code
    SyxObject *_syx_memory;
    SyxObject **_syx_freed_memory;
    \endcode

    The idea is to keep a stack containing free memory pointers. Once we free an object, we push on top of the stack of freed locations When we allocate a new SyxObject, we just pop the pointer from this stack.

    Take a look at syx-memory.c for more detailed informations.
    \note the SyxObject::data field, containing SyxObject pointers, is allocated outside this memory.
*/

#define DEBUG_GC

void
syx_memory_init (syx_varsize mem_size)
{
  if (_syx_memory_initialized)
    {
      if (mem_size > _syx_memory_size)
	syx_realloc (syx_memory, sizeof (SyxObject) * mem_size);

      return;
    }

  _syx_memory_size = mem_size;

  syx_memory = syx_calloc (_syx_memory_size, sizeof (SyxObject));
  _syx_freed_memory = syx_calloc (_syx_memory_size, sizeof (SyxOop));

  _syx_memory_initialized = TRUE;
}

void
syx_memory_clear (void)
{
  if (!_syx_memory_initialized)
    return;

  syx_varsize i;
  SyxObject *object = syx_memory;

  for (i=0; i < _syx_memory_size; i++, object++)
    {
      if (object->data)
	syx_free (object->data);
    }

  syx_free (syx_memory);
  syx_free (_syx_freed_memory);
  _syx_memory_initialized = FALSE;
}

SyxOop
syx_memory_alloc (void)
{
  SyxOop oop;

  if (_syx_memory_top >= _syx_memory_size - 1 && _syx_freed_memory_top == 0)
    {
      syx_memory_gc ();
      if (_syx_memory_top >= _syx_memory_size - 1 && _syx_freed_memory_top == 0)
	g_error ("object memory heap is full\n");
    }

  if (_syx_freed_memory_top == 0)
    oop.idx = _syx_memory_top++;
  else
    oop = _syx_freed_memory[--_syx_freed_memory_top];

  /* Prevent the object from being collected */
  if (_syx_memory_gc_trans_running)
    {
      SYX_OBJECT_IS_MARKED(oop) = TRUE;
      _syx_memory_gc_trans[(syx_int32) _syx_memory_gc_trans_top++] = oop;
    }

  return oop;
}

inline void
syx_memory_free (SyxOop oop)
{
  bzero (syx_memory + oop.idx, sizeof (SyxObject));
  _syx_freed_memory[_syx_freed_memory_top++] = oop;
}

inline void
syx_memory_free_all (void)
{
  syx_varsize i;
  SyxObject *object = syx_memory;

  // skip nil, true and false constants
  for (i=3; i < _syx_memory_size; i++, object++)
    {
      if (object)
	syx_free (object->data);
    }

  bzero (syx_memory, sizeof(SyxObject) * _syx_memory_size);
  _syx_freed_memory_top = 0;
  _syx_memory_top = 3;
}

inline syx_varsize
syx_memory_get_size (void)
{
  return _syx_memory_size;
}

inline SyxOop
SYX_POINTER_TO_OOP (syx_pointer ptr)
{
  SyxOop oop;
  oop.idx = ((syx_nint)ptr - (syx_nint)syx_memory) / sizeof (SyxObject);
  oop.c.type = SYX_TYPE_OBJECT;
  return oop;
}

inline void
_syx_memory_gc_mark (SyxOop object)
{
  syx_varsize i;
  if (!SYX_IS_OBJECT (object) || SYX_OBJECT_IS_MARKED(object) || SYX_IS_NIL(syx_object_get_class (object)))
    return;

  SYX_OBJECT_IS_MARKED(object) = TRUE;

  _syx_memory_gc_mark (SYX_OBJECT(object)->class);

  if (SYX_OBJECT_HAS_REFS (object))
    {
      for (i=0; i < SYX_OBJECT_SIZE (object); i++)
	_syx_memory_gc_mark (SYX_OBJECT_DATA(object)[i]);
    }
}

static void
_syx_memory_gc_sweep ()
{
  syx_varsize i;
  SyxObject *object = syx_memory + 3; // skip constants
  SyxOop oop;

  // skip constants
  for (i=3; i < _syx_memory_size; i++, object++)
    {
      if (SYX_IS_NIL(object->class))
	continue;

      if (object->is_marked)
	object->is_marked = FALSE;
      else
	{
	  oop.idx = i;
	  syx_object_free (oop);
	}
    }
}

/*!
  Calls the Syx garbage collector
 */
inline void
syx_memory_gc (void)
{
#ifdef DEBUG_GC
  syx_varsize old_top = _syx_freed_memory_top;
  syx_varsize reclaimed;
#endif

  SYX_OBJECT_IS_MARKED(syx_symbols) = TRUE;
  SYX_OBJECT_IS_MARKED(SYX_DICTIONARY_HASH_TABLE(syx_symbols)) = TRUE;
  _syx_memory_gc_mark (syx_globals);
  _syx_memory_gc_sweep ();

#ifdef DEBUG_GC
  reclaimed = _syx_freed_memory_top - old_top;
  printf("GC: reclaimed %d (%d%%) objects over %d\n", reclaimed, reclaimed * 100 / _syx_memory_size, _syx_memory_size);
#endif
}

/*!
  Begins a garbage collection transaction.
  In this transaction, all objects being allocated must not be freed.
  The transaction can hold up to 255 objects.
*/
inline void
syx_memory_gc_begin (void)
{
  _syx_memory_gc_trans_running = TRUE;
}

/*!
  Release a transaction started with syx_memory_gc_begin
*/
inline void
syx_memory_gc_end (void)
{
  syx_int32 i;

  if (!_syx_memory_gc_trans_running)
    return;

  for (i=0; i < _syx_memory_gc_trans_top; i++)
    SYX_OBJECT_IS_MARKED(_syx_memory_gc_trans[i]) = FALSE;

  _syx_memory_gc_trans_top = 0;
  _syx_memory_gc_trans_running = FALSE;
}

inline syx_pointer
syx_malloc (syx_varsize size)
{
  syx_pointer ptr;

  ptr = malloc (size);
  if (!ptr)
    g_error ("out of memory");

  return ptr;
}

inline syx_pointer
syx_malloc0 (syx_varsize size)
{
  syx_pointer ptr;

  ptr = malloc (size);
  if (!ptr)
    g_error ("out of memory");

  bzero (ptr, size);
  return ptr;
}

inline syx_pointer
syx_calloc (syx_varsize elements, syx_varsize element_size)
{
  syx_pointer ptr;

  ptr = calloc (elements, element_size);
  if (!ptr)
    g_error ("out of memory");

  return ptr;
}

inline syx_pointer
syx_realloc (syx_pointer ptr, syx_varsize size)
{
  syx_pointer nptr;

  nptr = realloc (ptr, size);
  if (!nptr)
    g_error ("out of memory");

  return nptr;
}

void
syx_freev (syx_pointer *ptrv)
{
  syx_pointer *ptrv_p = ptrv;

  while (*ptrv_p)
    syx_free (*ptrv_p++);

  syx_free (ptrv);
}

syx_bool
syx_memory_save_image (syx_symbol path)
{
  SyxObject *object;
  syx_varsize i;
  FILE *image;
  if (!path)
    return FALSE;

  object = syx_memory;
  image = fopen (path, "wb");
  if (!image)
    return FALSE;
  
  fwrite (&_syx_memory_size, sizeof (syx_varsize), 1, image);
  fwrite (&_syx_memory_top, sizeof (syx_varsize), 1, image);
  fwrite (&_syx_freed_memory_top, sizeof (syx_varsize), 1, image);
  fwrite (_syx_freed_memory, sizeof (SyxOop), _syx_freed_memory_top, image);
  fwrite (&syx_globals.idx, sizeof (syx_int32), 1, image);
  fwrite (&syx_symbols.idx, sizeof (syx_int32), 1, image);

  syx_memory_gc ();


  for (i=0; i < _syx_memory_size; i++, object++)
    {
      if (SYX_IS_NIL (object->class))
	continue;

      fwrite (&i, sizeof (syx_int32), 1, image);
      fwrite (&object->class.idx, sizeof (syx_int32), 1, image);
      fputc (object->has_refs, image);
      fwrite (&object->size, sizeof (syx_varsize), 1, image);

      if (object->size > 0)
	{
	  if (object->has_refs)
	    fwrite (object->data, sizeof (SyxOop), object->size, image);
	  else
	    fwrite (object->data, sizeof (syx_int8), object->size, image);
	}
    }

  fclose (image);

  return TRUE;
}

syx_bool
syx_memory_load_image (syx_symbol path)
{
  SyxObject *object;
  syx_int32 index;
  FILE *image;
  syx_varsize mem_size;

  if (!path)
    return FALSE;

  image = fopen (path, "rb");
  if (!image)
    return FALSE;

  fread (&mem_size, sizeof (syx_varsize), 1, image);
  syx_memory_init (mem_size);

  fread (&_syx_memory_top, sizeof (syx_varsize), 1, image);
  fread (&_syx_freed_memory_top, sizeof (syx_varsize), 1, image);
  fread (_syx_freed_memory, sizeof (SyxOop), _syx_freed_memory_top, image);
  fread (&syx_globals.idx, sizeof (syx_int32), 1, image);
  fread (&syx_symbols.idx, sizeof (syx_int32), 1, image);

  while (!feof (image))
    {
      fread (&index, sizeof (syx_int32), 1, image);
      object = syx_memory + index;

      fread (&object->class.idx, sizeof (syx_int32), 1, image);
      object->has_refs = fgetc (image);
      fread (&object->size, sizeof (syx_varsize), 1, image);
      
      if (object->size > 0)
	{
	  if (object->has_refs)
	    {
	      if (object->data)
		syx_free (object->data);

	      object->data = syx_calloc (object->size, sizeof (SyxOop));
	      fread (object->data, sizeof (SyxOop), object->size, image);
	    }
	  else
	    {
	      if (object->data)
		syx_free (object->data);

	      object->data = syx_calloc (object->size, sizeof (syx_int8));
	      fread (object->data, sizeof (syx_int8), object->size, image);
	    }
	}
	
    }

  fclose (image);

  syx_fetch_basic ();

  return TRUE;
}
