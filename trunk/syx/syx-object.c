#include <stdio.h>
#include "syx-object.h"
#include "syx-memory.h"
#include "syx-enums.h"
#include "syx-types.h"
#include "syx-scheduler.h"

/*! \page syx_object Syx Object
  
  \section Description
  syx-object.c: this file contains all the functions needed to work with Smalltalk objects.
  
  Objects are the core of Syx. They can represent both instances or classes.
  \note all objects are allocated in the Syx Memory
*/
 
SyxOop syx_nil,
  syx_true,
  syx_false,

  syx_metaclass_class,
  syx_undefined_object_class,
  syx_true_class,
  syx_false_class,
  syx_small_integer_class,
  syx_character_class,

  syx_symbol_class,
  syx_string_class,
  syx_byte_array_class,
  syx_array_class,

  syx_link_class,
  syx_dictionary_class,

  syx_compiled_method_class,
  syx_compiled_block_class,
  syx_block_closure_class,

  syx_method_context_class,
  syx_block_context_class,
  syx_process_class,
  syx_processor_scheduler_class,

  syx_symbols,
  syx_globals;

/* Inlines */

inline void
syx_object_grow_by (SyxOop object, syx_varsize size)
{
  SYX_OBJECT_DATA(object) = syx_realloc (SYX_OBJECT_DATA(object), SYX_OBJECT_SIZE(object) + size);
}

inline SyxOop 
syx_object_get_class (SyxOop object)
{
  /* ordered by usage */
  
  if (SYX_IS_OBJECT(object))
    return SYX_OBJECT(object)->class;

  if (SYX_IS_SMALL_INTEGER(object))
    return syx_small_integer_class;

  if (SYX_IS_CHARACTER(object))
    return syx_character_class;
    
  /* it's a constant */
  return SYX_OBJECT(object)->class;
}

inline void
syx_object_set_class (SyxOop object, SyxOop val)
{
  if (!SYX_IS_OBJECT(object))
    return;

  SYX_OBJECT(object)->class = val;
}

inline syx_int32
syx_object_hash (SyxOop object)
{
  if (SYX_IS_SMALL_INTEGER (object))
    return SYX_SMALL_INTEGER (object);
  else if (SYX_IS_CHARACTER (object))
    return SYX_CHARACTER (object);

  return object.idx;
}

/* Builders */

inline SyxOop 
syx_metaclass_new (SyxOop supermetaclass)
{
  SyxOop metaclass = syx_object_new (syx_metaclass_class, TRUE);
  

  SYX_CLASS_SUPERCLASS(metaclass) = supermetaclass;
  SYX_CLASS_INSTANCE_SIZE(metaclass) = SYX_CLASS_INSTANCE_SIZE(supermetaclass);
  SYX_CLASS_INSTANCE_VARIABLES(metaclass) = syx_array_new (0, NULL);
  return metaclass;
}

inline SyxOop 
syx_class_new (SyxOop superclass)
{
  SyxOop metaclass = syx_metaclass_new (syx_object_get_class (superclass));
  SyxOop class = syx_object_new (metaclass, TRUE);
  SYX_CLASS_SUPERCLASS(class) = superclass;
  SYX_CLASS_INSTANCE_SIZE(class) = SYX_CLASS_INSTANCE_SIZE(superclass);
  SYX_CLASS_INSTANCE_VARIABLES(class) = syx_array_new (0, NULL);
  return class;
}

inline SyxOop 
syx_byte_array_new (syx_varsize size, syx_uint8 *data)
{
  return syx_object_new_data (syx_byte_array_class, FALSE, size, (SyxOop *)data);
}

inline SyxOop 
syx_byte_array_new_size (syx_varsize size)
{
  SyxOop oop = syx_memory_alloc ();
  SyxObject *object = SYX_OBJECT (oop);
  
  object->class = syx_byte_array_class;
  object->has_refs = FALSE;
  object->size = size;
  object->data = syx_calloc (size, sizeof (syx_uint8));

  return oop;
}

inline SyxOop
syx_byte_array_new_ref (syx_varsize size, syx_uint8 *data)
{
  SyxOop oop = syx_byte_array_new_size (size);
  memcpy (SYX_OBJECT_DATA (oop), data, size * sizeof (syx_uint8));
  return oop;
}

inline SyxOop 
syx_array_new (syx_varsize size, SyxOop *data)
{
  return syx_object_new_data (syx_array_class, TRUE, size, data);
}

inline SyxOop 
syx_array_new_size (syx_varsize size)
{
  return syx_object_new_size (syx_array_class, TRUE, size);
}

inline SyxOop
syx_array_new_ref (syx_varsize size, SyxOop *data)
{
  SyxOop oop = syx_array_new_size (size);
  memcpy (SYX_OBJECT_DATA(oop), data, size * sizeof (SyxOop));
  return oop;
}

inline SyxOop 
syx_symbol_new (syx_symbol symbol)
{
  SyxOop object = syx_dictionary_at_symbol (syx_symbols, symbol);
  if (SYX_IS_NIL (object))
    {
      object = syx_object_new_data (syx_symbol_class, FALSE, strlen (symbol), (SyxOop *)strdup (symbol));
      syx_dictionary_at_const_put (syx_symbols, object, object);
    }

  return object;
}

inline SyxOop 
syx_string_new (syx_symbol string)
{
  return syx_object_new_data (syx_string_class, FALSE, strlen (string), (SyxOop *)strdup (string));
}

inline SyxOop 
syx_link_new (SyxOop key, SyxOop value)
{
  SyxOop object = syx_object_new (syx_link_class, TRUE);
  SYX_LINK_KEY(object) = key;
  SYX_LINK_VALUE(object) = value;
  return object;
}

SyxOop 
syx_dictionary_new (syx_varsize size)
{
  SyxOop object = syx_object_new (syx_dictionary_class, TRUE);
  SYX_DICTIONARY_HASH_TABLE(object) = syx_array_new_size (size * 2);
  return object;
}

SyxOop 
syx_dictionary_at_const (SyxOop dict, SyxOop key)
{
  SyxOop table = SYX_DICTIONARY_HASH_TABLE (dict);
  syx_varsize size = SYX_OBJECT_SIZE (table);
  syx_varsize i;

  for (i=0; i < size; i+=2)
    {
      if (SYX_OOP_EQ(SYX_OBJECT_DATA(table)[i], key))
	return SYX_OBJECT_DATA(table)[i+1];
    }
  
  return SYX_NIL;
}

SyxOop 
syx_dictionary_at_symbol (SyxOop dict, syx_symbol key)
{
  SyxOop table = SYX_DICTIONARY_HASH_TABLE (dict);
  SyxOop entry;
  syx_varsize size = SYX_OBJECT_SIZE (table);
  syx_varsize i;

  for (i=0; i < size; i+=2)
    {
      entry = SYX_OBJECT_DATA(table)[i];
      if (!SYX_IS_NIL (entry) && !strcmp (SYX_OBJECT_SYMBOL (entry), key))
	return SYX_OBJECT_DATA(table)[i+1];
    }
  
  return SYX_NIL;
}

void
syx_dictionary_at_const_put (SyxOop dict, SyxOop key, SyxOop value)
{
  SyxOop table = SYX_DICTIONARY_HASH_TABLE (dict);
  syx_varsize size = SYX_OBJECT_SIZE (table);
  syx_varsize i;

  for (i=0; i < size; i+=2)
    {
      if (SYX_IS_NIL (SYX_OBJECT_DATA(table)[i]) || SYX_OOP_EQ(SYX_OBJECT_DATA(table)[i], key))
	{
	  SYX_OBJECT_DATA(table)[i] = key;
	  SYX_OBJECT_DATA(table)[i+1] = value;
	  return;
	}
    }

  printf("Not enough space for dictionary %d\n", dict.idx);
}

inline SyxOop 
syx_block_closure_new (SyxOop block)
{
  SyxOop object = syx_object_new (syx_block_closure_class, TRUE);
  SYX_BLOCK_CLOSURE_BLOCK(object) = block;
  return object;
}

inline SyxOop 
syx_process_new (SyxOop context)
{
  SyxOop object = syx_object_new (syx_process_class,  TRUE);
  SYX_PROCESS_CONTEXT(object) = context;
  SYX_PROCESS_SUSPENDED(object) = SYX_TRUE;
  SYX_PROCESS_SCHEDULED(object) = SYX_FALSE;
  syx_scheduler_add_process (object);
  return object;
}

inline SyxOop 
syx_method_context_new (SyxOop parent, SyxOop method, SyxOop receiver, SyxOop arguments)
{
  syx_memory_gc_begin ();

  SyxOop object = syx_object_new (syx_method_context_class, TRUE);
  SyxOop ctx_args;

  SYX_METHOD_CONTEXT_PARENT(object) = parent;
  SYX_METHOD_CONTEXT_METHOD(object) = method;
  SYX_METHOD_CONTEXT_RECEIVER(object) = receiver;

  SYX_METHOD_CONTEXT_ARGUMENTS(object) = ctx_args = syx_array_new_size (SYX_SMALL_INTEGER(SYX_METHOD_ARGUMENTS_COUNT (method)));
  memcpy (SYX_OBJECT_DATA(ctx_args), SYX_OBJECT_DATA(arguments), SYX_OBJECT_SIZE(arguments) * sizeof (SyxOop ));

  SYX_METHOD_CONTEXT_TEMPORARIES(object) = syx_array_new_size (SYX_SMALL_INTEGER(SYX_METHOD_TEMPORARIES_COUNT (method)));
  SYX_METHOD_CONTEXT_IP(object) = syx_small_integer_new (0);
  SYX_METHOD_CONTEXT_SP(object) = syx_small_integer_new (0);
  SYX_METHOD_CONTEXT_STACK(object) = syx_array_new_size (SYX_SMALL_INTEGER(SYX_METHOD_STACK_SIZE (method)));

  SYX_METHOD_CONTEXT_RETURN_CONTEXT(object) = parent;

  syx_memory_gc_end ();

  return object;
}

inline SyxOop 
syx_block_context_new (SyxOop parent, SyxOop block, SyxOop arguments, SyxOop outer_context)
{
  syx_memory_gc_begin ();

  SyxOop object = syx_object_new (syx_block_context_class, TRUE);
  SyxOop ctx_args;

  SYX_METHOD_CONTEXT_PARENT(object) = parent;
  SYX_METHOD_CONTEXT_METHOD(object) = block;
  SYX_METHOD_CONTEXT_RECEIVER(object) = SYX_METHOD_CONTEXT_RECEIVER (outer_context);

  SYX_METHOD_CONTEXT_ARGUMENTS(object) = ctx_args = SYX_METHOD_CONTEXT_ARGUMENTS (outer_context);
  memcpy (SYX_OBJECT_DATA(ctx_args) + SYX_SMALL_INTEGER(SYX_BLOCK_ARGUMENTS_TOP(block)),
	  SYX_OBJECT_DATA(arguments), SYX_OBJECT_SIZE(arguments) * sizeof (SyxOop ));

  SYX_METHOD_CONTEXT_TEMPORARIES(object) = SYX_METHOD_CONTEXT_TEMPORARIES(outer_context);
  SYX_METHOD_CONTEXT_IP(object) = syx_small_integer_new (0);
  SYX_METHOD_CONTEXT_STACK(object) = syx_array_new_size (SYX_SMALL_INTEGER(SYX_METHOD_STACK_SIZE (block)));

  SYX_BLOCK_CONTEXT_OUTER_CONTEXT(object) = outer_context;
  SYX_METHOD_CONTEXT_RETURN_CONTEXT(object) = SYX_METHOD_CONTEXT_RETURN_CONTEXT (outer_context);

  syx_memory_gc_end ();

  return object;
}

/* Object */

SyxOop 
syx_object_new (SyxOop class, syx_bool has_refs)
{
  SyxOop oop = syx_memory_alloc ();
  SyxObject *object = SYX_OBJECT (oop);
  
  object->class = class;
  object->has_refs = has_refs;
  object->size = SYX_SMALL_INTEGER (SYX_CLASS_INSTANCE_SIZE (class));
  object->data = object->size ? syx_calloc (object->size, sizeof (SyxObject)) : NULL;

  return oop;
}

SyxOop 
syx_object_new_size (SyxOop class, syx_bool has_refs, syx_varsize size)
{
  SyxOop oop = syx_memory_alloc ();
  SyxObject *object = SYX_OBJECT (oop);
  
  object->class = class;
  object->has_refs = has_refs;
  object->size = size;
  object->data = size ? syx_calloc (size, sizeof (SyxOop )) : NULL;

  return oop;
}

SyxOop 
syx_object_new_data (SyxOop class, syx_bool has_refs, syx_varsize size, SyxOop *data)
{
  SyxOop oop = syx_memory_alloc ();
  SyxObject *object = SYX_OBJECT (oop);

  object->class = class;
  object->has_refs = has_refs;
  object->size = size;
  object->data = data;

  return oop;
}

inline void
syx_object_free (SyxOop object)
{
  syx_free (SYX_OBJECT_DATA (object));
  syx_memory_free (object);
}

inline SyxOop
syx_small_integer_new (syx_int32 num)
{
  SyxOop oop;
  oop.idx = num;
  oop.i.type = SYX_TYPE_SMALL_INTEGER;
  return oop;
}

inline SyxOop
syx_character_new (syx_uint8 ch)
{
  SyxOop oop;
  oop.idx = ch;
  oop.c.type = SYX_TYPE_CHARACTER;
  return oop;
}

syx_bool
syx_class_is_superclass_of (SyxOop class, SyxOop subclass)
{
  if (SYX_OOP_EQ (class, subclass))
    return FALSE;

  SyxOop cur = SYX_CLASS_SUPERCLASS (subclass);

  for (; !SYX_IS_NIL (cur) && SYX_OOP_NE(cur, class); cur=SYX_CLASS_SUPERCLASS(cur));

  return !SYX_IS_NIL (cur);
}

syx_symbol *
syx_class_get_all_instance_variables (SyxOop class)
{
  syx_symbol names[256];
  syx_symbol *ret_names = NULL;
  SyxOop inst_vars;
  syx_varsize i, size, tot_size;

  for (tot_size=0; !SYX_IS_NIL(class); class=SYX_CLASS_SUPERCLASS (class))
    {
      inst_vars = SYX_CLASS_INSTANCE_VARIABLES (class);
      size = SYX_OBJECT_SIZE (inst_vars);

      for (i=size; i > 0; i--)
	{
	  tot_size++;
	  names[255-tot_size+1] = SYX_OBJECT_SYMBOL (SYX_OBJECT_DATA(inst_vars)[i-1]);
	}
    }
  if (tot_size > 0)
    {
      ret_names = syx_calloc (tot_size + 1, sizeof (syx_symbol));
      memcpy (ret_names, &names[255-tot_size+1], tot_size * sizeof (syx_symbol));
    }
  return ret_names;
}

SyxOop 
syx_class_lookup_method (SyxOop class, syx_symbol selector)
{
  SyxOop cur;
  SyxOop method;

  for (cur=class; !SYX_IS_NIL (cur); cur = SYX_CLASS_SUPERCLASS (cur))
    {
      if (SYX_IS_NIL (SYX_CLASS_METHODS (cur)))
	continue;

      method = syx_dictionary_at_symbol (SYX_CLASS_METHODS (cur), selector);
      if (!SYX_IS_NIL (method))
	return method;
    }

  return SYX_NIL;
}
