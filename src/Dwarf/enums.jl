@enum Tag::UInt8 begin
  TAG_array_type = 0x01
  TAG_class_type
  TAG_entry_point
  TAG_enumeration_type
  TAG_formal_parameter
  TAG_imported_declaration
  TAG_label = 0x0a
  TAG_lexical_block
  TAG_member = 0x0d
  TAG_pointer_type = 0x0f
  TAG_reference_type
  TAG_compile_unit
  TAG_string_type
  TAG_structure_type
  TAG_subroutine_type = 0x15
  TAG_typedef
  TAG_union_type
  TAG_unspecified_parameters
  TAG_variant
  TAG_common_block
  TAG_common_inclusion
  TAG_inheritance
  TAG_inlined_subroutine
  TAG_module
  TAG_ptr_to_member_type
  TAG_set_type
  TAG_subrange_type
  TAG_with_stmt
  TAG_access_declaration
  TAG_base_type
  TAG_catch_block
  TAG_const_type
  TAG_constant
  TAG_enumerator
  TAG_file_type
  TAG_friend
  TAG_namelist
  TAG_namelist_item
  TAG_packed_typed
  TAG_subprogram
  TAG_template_type_parameter
  TAG_template_value_parameter
  TAG_thrown_type
  TAG_try_block
  TAG_variant_part
  TAG_variable
  TAG_volatile_type
  TAG_dwarf_procedure
  TAG_restrict_type
  TAG_interface_type
  TAG_namespace
  TAG_imported_module
  TAG_unspecified_type
  TAG_partial_unit
  TAG_imported_unit
  TAG_condition = 0x3f
  TAG_shared_type
  TAG_type_unit
  TAG_rvalue_reference_type
  TAG_template_alias
end

@enum Attr::UInt8 begin
  AT_sibling = 0x01
  AT_location
  AT_name
  AT_ordering = 0x09
  AT_byte_size = 0x0b
  AT_bit_offset
  AT_bit_size
  AT_stmt_list = 0x10
  AT_low_pc
  AT_high_pc
  AT_language
  AT_discr = 0x15
  AT_discr_value
  AT_visibility
  AT_import
  AT_string_length
  AT_common_reference
  AT_comp_dir
  AT_const_value
  AT_containing_type
  AT_default_value
  AT_inline = 0x20
  AT_is_optional
  AT_lower_bound
  AT_producer = 0x25
  AT_prototyped = 0x27
  AT_return_addr = 0x2a
  AT_start_scope = 0x2c
  AT_bit_stride = 0x2e
  AT_upper_bound
  AT_abstract_origin = 0x31
  AT_accessibility
  AT_address_class
  AT_artificial
  AT_base_types
  AT_calling_convention
  AT_count
  AT_data_member_location
  AT_decl_column
  AT_decl_file
  AT_decl_line
  AT_declaration
  AT_discr_list
  AT_encoding
  AT_external
  AT_frame_base
  AT_friend
  AT_identifier_case
  AT_macro_info
  AT_namelist_item
  AT_priority
  AT_segment
  AT_specification
  AT_static_link
  AT_type
  AT_use_location
  AT_variable_parameter
  AT_virtuality
  AT_vtable_elem_location
  AT_allocated
  AT_associated
  AT_data_location
  AT_byte_stride
  AT_entry_pc
  AT_use_UTF8
  AT_extension
  AT_ranges
  AT_trampoline
  AT_call_column
  AT_call_file
  AT_call_line
  AT_description
  AT_binary_scale
  AT_decimal_scale
  AT_small
  AT_decimal_sign
  AT_digit_count
  AT_picture_string
  AT_mutable
  AT_threads_scaled
  AT_explicit
  AT_object_pointer
  AT_endianity
  AT_elemental
  AT_pure
  AT_recursive
  AT_signature
  AT_main_subprogram
  AT_data_bit_offset
  AT_const_expr
  AT_enum_class
  AT_linkage_name
end

@enum Form::UInt8 begin
  FORM_addr = 0x01
  FORM_block2 = 0x03
  FORM_block4
  FORM_data2
  FORM_data4
  FORM_data8
  FORM_string
  FORM_block
  FORM_block1
  FORM_data1
  FORM_flag
  FORM_sdata
  FORM_strp
  FORM_udata
  FORM_ref_addr
  FORM_ref1
  FORM_ref2
  FORM_ref4
  FORM_ref8
  FORM_ref_udata
  FORM_indirect
  FORM_sec_offset
  FORM_exprloc
  FORM_flag_present
  FORM_ref_sig8 = 0x20
end
