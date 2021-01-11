CLASS zmal_cl_sfut_email_excel DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,
      create_excel
        IMPORTING
          VALUE(delinq_table_raw) TYPE zmal_sfut_supplier_data_t
        EXPORTING
          ex_rawdata              TYPE solix_tab
          ex_bytecount            TYPE sood-objlen,
      download_frontend
        IMPORTING
          im_path TYPE string
          im_file TYPE string.
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: excel                         TYPE REF TO zcl_excel,
          worksheet                     TYPE REF TO zcl_excel_worksheet,
          worksheet_range               TYPE REF TO zcl_excel_range,
          style_darkgreen_guid          TYPE zexcel_cell_style,
          style_yellow_guid             TYPE zexcel_cell_style,
          style_red_guid                TYPE zexcel_cell_style,
          style_unprotected_guid        TYPE zexcel_cell_style,
          style_unprotected_yellow_guid TYPE zexcel_cell_style,
          style_unprotected_red_guid    TYPE zexcel_cell_style,
          delinq_table                  TYPE STANDARD TABLE OF
                                          zmal_sfut_supplier_data.

    METHODS: protect_current_worksheet,

      create_styles,

      create_range
        IMPORTING
          im_tab_range_values TYPE string_table
          im_range_name       TYPE string            " Do not use spaces
          im_start_col        TYPE char2    DEFAULT 'A'
          im_start_row        TYPE i        DEFAULT 1
        EXPORTING
          ex_end_row          TYPE i,


      create_date_str_table
        IMPORTING
          im_start_date     TYPE sy-datum
          im_range_days     TYPE i
        EXPORTING
          ex_tab_range_date TYPE string_table,

      create_dropdown
        IMPORTING
          im_range_name TYPE string
          im_value      TYPE string
          im_row        TYPE i
          im_col        TYPE char2
          im_style      TYPE zexcel_cell_style,

      set_worksheet_cols_auto_size
        IMPORTING
          im_start_col TYPE char2
          im_end_col   TYPE char2,

      create_fieldcatalog
        IMPORTING
          im_table           TYPE ANY TABLE
        RETURNING
          VALUE(re_fieldcat) TYPE zexcel_t_fieldcatalog,

      apply_style_row
        IMPORTING
          im_row       TYPE i
          im_start_col TYPE char2
          im_end_col   TYPE char2
          im_style     TYPE zexcel_cell_style,
      create_params_sheet,
      create_forms_sheet,
      create_attachment
        EXPORTING
          ex_rawdata   TYPE solix_tab
          ex_bytecount TYPE sood-objlen.
ENDCLASS.



CLASS zmal_cl_sfut_email_excel IMPLEMENTATION.
  METHOD constructor.

    " Create ABAP2XLSX instance
    me->excel = NEW zcl_excel( ).

    me->create_styles( ).

  ENDMETHOD.

  METHOD create_styles.

    DATA: lo_style TYPE REF TO zcl_excel_style.

    " Create all styles
    lo_style = excel->add_new_style( ).
    lo_style->protection->locked =
      zcl_excel_style_protection=>c_protection_unlocked.
    me->style_unprotected_guid = lo_style->get_guid( ).

    lo_style = excel->add_new_style( ).
    lo_style->fill->filltype = zcl_excel_style_fill=>c_fill_solid.
    lo_style->fill->fgcolor-rgb = zcl_excel_style_color=>c_yellow.
    me->style_yellow_guid = lo_style->get_guid( ).

    lo_style = excel->add_new_style( ).
    lo_style->fill->filltype = zcl_excel_style_fill=>c_fill_solid.
    lo_style->fill->fgcolor-rgb = zcl_excel_style_color=>c_red.
    lo_style->font->color-rgb = zcl_excel_style_color=>c_white.
    me->style_red_guid = lo_style->get_guid( ).

    lo_style = excel->add_new_style( ).
    lo_style->fill->filltype = zcl_excel_style_fill=>c_fill_solid.
    lo_style->fill->fgcolor-rgb = zcl_excel_style_color=>c_yellow.
    lo_style->protection->locked =
      zcl_excel_style_protection=>c_protection_unlocked.
    me->style_unprotected_yellow_guid = lo_style->get_guid( ).

    lo_style = excel->add_new_style( ).
    lo_style->fill->filltype = zcl_excel_style_fill=>c_fill_solid.
    lo_style->fill->fgcolor-rgb = zcl_excel_style_color=>c_red.
    lo_style->font->color-rgb = zcl_excel_style_color=>c_white.
    lo_style->protection->locked =
      zcl_excel_style_protection=>c_protection_unlocked.
    me->style_unprotected_red_guid = lo_style->get_guid( ).

    lo_style = excel->add_new_style( ).
    lo_style->fill->filltype = zcl_excel_style_fill=>c_fill_solid.
    lo_style->fill->fgcolor-rgb = zcl_excel_style_color=>c_darkgreen.
    me->style_darkgreen_guid = lo_style->get_guid( ).

  ENDMETHOD.

  METHOD create_excel.

    me->delinq_table[] = delinq_table_raw[].

    me->create_params_sheet( ).

    me->create_forms_sheet( ).

    me->create_attachment(
      IMPORTING
        ex_rawdata   = ex_rawdata
        ex_bytecount = ex_bytecount
    ).

  ENDMETHOD.

  METHOD create_params_sheet.
    DATA: lv_date_str_range TYPE string_table,
          lv_row            TYPE i.

    me->worksheet = me->excel->get_active_worksheet( ).
    me->worksheet->set_title( 'Parameters' ).

    " Create range of acceptance options
    me->create_range(
        EXPORTING
            im_range_name = 'Response'
            im_tab_range_values = VALUE #(
                ( |Accept| )
                ( |Reject| )
                ( |Partial| )
            )
        IMPORTING
            ex_end_row = lv_row
    ).

    " Create internal table for all dates
    me->create_date_str_table(
        EXPORTING
            im_range_days = 365
            im_start_date = sy-datum
        IMPORTING
            ex_tab_range_date = lv_date_str_range ).

    " Create range of dates
    me->create_range(
        im_range_name = 'Dates'
        im_tab_range_values = lv_date_str_range
        im_start_row = lv_row + 1
    ).
    " Free alocated date table as it is not used anymore
    FREE lv_date_str_range.

    me->protect_current_worksheet( ).

    me->worksheet->zif_excel_sheet_properties~zoomscale = 100.
    me->worksheet->zif_excel_sheet_properties~hidden = abap_true.


  ENDMETHOD.

  METHOD create_forms_sheet.
    DATA lv_is_table_settings TYPE zexcel_s_table_settings.
    DATA lv_tab_index TYPE sy-tabix.

    DATA: lv_editable_style_color_guid TYPE zexcel_cell_style.

    DATA: lo_data_validation TYPE REF TO zcl_excel_data_validation,
          lv_delinq_fieldcat TYPE zexcel_t_fieldcatalog.

    me->worksheet = me->excel->add_new_worksheet( ).
    me->worksheet->set_title( 'Form' ).

    " Set table settings
    lv_is_table_settings-show_row_stripes = abap_true.
    lv_is_table_settings-top_left_column = 'A'.
    lv_is_table_settings-top_left_row = 1.
    lv_is_table_settings-table_style = 'TableStyleMedium13'.


    me->protect_current_worksheet( ).

    lv_delinq_fieldcat = create_fieldcatalog(
      im_table = me->delinq_table
    ).

    me->worksheet->bind_table(
      EXPORTING
        ip_table          = me->delinq_table
        it_field_catalog  = lv_delinq_fieldcat " Table binding fcat
        is_table_settings = lv_is_table_settings " Excel table settings
    ).

    " Apply style on editable header cols
    me->apply_style_row(
      EXPORTING
        im_row       = 1
        im_start_col = 'N'
        im_end_col   = 'S'
        im_style     = me->style_darkgreen_guid
    ).

    " Add editable options (dropdowns and fields)
    LOOP AT me->delinq_table ASSIGNING FIELD-SYMBOL(<delinq_item>).
      " Excel table index
      lv_tab_index = sy-tabix + 1.

      lv_editable_style_color_guid = me->style_unprotected_guid.

      " Conditional styling on status
      IF <delinq_item>-ship_status = 'Should Have Shipped,But Has Not'.

        me->apply_style_row(
          EXPORTING
            im_row       = lv_tab_index
            im_start_col = 'A'
            im_end_col   = 'S'
            im_style     = me->style_red_guid
        ).

        lv_editable_style_color_guid = me->style_unprotected_red_guid.

      ELSEIF <delinq_item>-ship_status = 'Should Be Shipping Today'
          OR <delinq_item>-ship_status = 'Should Have Shipped, And Has'.

        me->apply_style_row(
          EXPORTING
            im_row       = lv_tab_index
            im_start_col = 'A'
            im_end_col   = 'S'
            im_style     = me->style_yellow_guid
        ).

        lv_editable_style_color_guid = me->style_unprotected_yellow_guid.

      ENDIF.

      me->create_dropdown(
        EXPORTING
          im_range_name = 'Response'
          im_value      = 'Select response'
          im_row        = lv_tab_index
          im_col        = 'N'
          im_style         = lv_editable_style_color_guid
      ).
      me->create_dropdown(
        EXPORTING
          im_range_name = 'Dates'
          im_value      = 'Select date'
          im_row        = lv_tab_index
          im_col        = 'O'
          im_style         = lv_editable_style_color_guid
      ).

      " Qty column
      me->worksheet->set_cell(
        ip_row = lv_tab_index
        ip_column = 'P'
        ip_value = ''
        ip_style = lv_editable_style_color_guid
      ).

      me->create_dropdown(
        EXPORTING
          im_range_name = 'Dates'
          im_value      = 'Select date'
          im_row        = lv_tab_index
          im_col        = 'Q'
          im_style         = lv_editable_style_color_guid
      ).

      " Qty column
      me->worksheet->set_cell(
        ip_row = lv_tab_index
        ip_column = 'R'
        ip_value = ''
        ip_style = lv_editable_style_color_guid
      ).

      " Comment column
      me->worksheet->set_cell(
        ip_row = lv_tab_index
        ip_column = 'S'
        ip_value = ''
        ip_style = lv_editable_style_color_guid
      ).

      " Validation for qty fields
      lo_data_validation = me->worksheet->add_new_data_validation( ).
      lo_data_validation->type = zcl_excel_data_validation=>c_type_whole.
      lo_data_validation->cell_row = lv_tab_index.
      lo_data_validation->cell_column = 'P'.

      lo_data_validation = me->worksheet->add_new_data_validation( ).
      lo_data_validation->type = zcl_excel_data_validation=>c_type_whole.
      lo_data_validation->cell_row = lv_tab_index.
      lo_data_validation->cell_column = 'R'.

    ENDLOOP.

    me->set_worksheet_cols_auto_size(
        im_start_col = 'A'
        im_end_col = 'S'
    ).

    " Set worksheet zoom
    me->worksheet->zif_excel_sheet_properties~zoomscale = 85.

  ENDMETHOD.

  METHOD create_attachment.
    DATA: cl_writer    TYPE REF TO zif_excel_writer,
          lv_xdata     TYPE xstring,
          lv_bytecount TYPE i.

    cl_writer = NEW zcl_excel_writer_2007( ).
    lv_xdata = cl_writer->write_file( excel ).
    ex_rawdata = cl_bcs_convert=>xstring_to_solix(
      iv_xstring  = lv_xdata
    ).
    lv_bytecount = xstrlen( lv_xdata ).
    ex_bytecount = lv_bytecount.

  ENDMETHOD.

  METHOD download_frontend.
    DATA: lv_filename    TYPE string,
          lv_t_rawdata   TYPE solix_tab,
          lv_bytecount   TYPE sood-objlen,
          lv_i_bytecount TYPE i.

    me->create_attachment(
      IMPORTING
        ex_rawdata   = lv_t_rawdata
        ex_bytecount = lv_bytecount
    ).

    lv_i_bytecount = lv_bytecount.

    lv_filename = im_path.
* Add trailing "\" or "/"
    IF lv_filename CA '/'.
      REPLACE REGEX '([^/])\s*$' IN lv_filename WITH '$1/' .
    ELSE.
      REPLACE REGEX '([^\\])\s*$' IN lv_filename WITH '$1\\'.
    ENDIF.

    CONCATENATE lv_filename im_file INTO lv_filename.
* Get trailing blank
    cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = lv_i_bytecount
                                                      filename     = lv_filename
                                                      filetype     = 'BIN'
                                             CHANGING data_tab     = lv_t_rawdata ).
  ENDMETHOD.

  METHOD protect_current_worksheet.
    IF me->worksheet IS NOT INITIAL.
      me->worksheet->zif_excel_sheet_protection~protected  =
        zif_excel_sheet_protection=>c_protected.
      me->worksheet->zif_excel_sheet_protection~password   =
        'DAA7'. "it is the encoded word "secret"
*      worksheet->zif_excel_sheet_protection~password   =
*        zcl_excel_common=>encrypt_password( p_pwd ).
      me->worksheet->zif_excel_sheet_protection~sheet      =
        zif_excel_sheet_protection=>c_active.
      me->worksheet->zif_excel_sheet_protection~objects    =
        zif_excel_sheet_protection=>c_active.
      me->worksheet->zif_excel_sheet_protection~scenarios  =
        zif_excel_sheet_protection=>c_active.
    ENDIF.
  ENDMETHOD.

  METHOD create_range.
    DATA: lv_start_col TYPE char2,
          lv_start_row TYPE i.
    DATA: lo_range TYPE REF TO zcl_excel_range,
          lv_title TYPE zexcel_sheet_title.

    lv_start_row = im_start_row.
    lv_start_col = im_start_col.

    IF im_tab_range_values IS NOT INITIAL
        AND im_range_name  IS NOT INITIAL.

      me->worksheet->set_cell(
        ip_row = lv_start_row
        ip_column = lv_start_col
        ip_value = im_range_name
      ).
      LOOP AT im_tab_range_values ASSIGNING FIELD-SYMBOL(<range_value>).
        lv_start_row = lv_start_row + 1.

        me->worksheet->set_cell(
          ip_row = lv_start_row
          ip_column = lv_start_col
          ip_value = <range_value>
        ).

      ENDLOOP.

      lv_title = excel->get_active_worksheet( )->get_title( ).

      lo_range            = me->excel->add_new_range( ).
      lo_range->name      = im_range_name.
      lo_range->set_value( ip_sheet_name    = lv_title
                           ip_start_column  = im_start_col
                           " + 1 to ignore header
                           ip_start_row     = im_start_row + 1
                           ip_stop_column   = im_start_col
                           ip_stop_row      = lv_start_row ).

      ex_end_row = lv_start_row.


    ENDIF.
  ENDMETHOD.

  METHOD create_date_str_table.
    DATA: lt_range_str_date TYPE string_table,
          lv_date_iterator  TYPE sy-datum,
          lv_end_date       TYPE sy-datum,
          lv_str_date       TYPE string.

    lv_date_iterator = im_start_date.
    lv_end_date = im_start_date + im_range_days.

    WHILE lv_date_iterator < lv_end_date.
      CLEAR lv_str_date.
      CONCATENATE lv_date_iterator+4(2)
                  lv_date_iterator+6(2)
                  lv_date_iterator(4)
                    INTO lv_str_date SEPARATED BY '/'.

      APPEND lv_str_date TO lt_range_str_date.

      lv_date_iterator = lv_date_iterator + 1.
    ENDWHILE.

    ex_tab_range_date[] = lt_range_str_date[].

    FREE lt_range_str_date.

  ENDMETHOD.

  METHOD create_dropdown.
    DATA: lo_data_validation   TYPE REF TO zcl_excel_data_validation.

    lo_data_validation = me->worksheet->add_new_data_validation( ).
    lo_data_validation->type = zcl_excel_data_validation=>c_type_list.
    lo_data_validation->formula1 = im_range_name.
    lo_data_validation->cell_row = im_row.
    lo_data_validation->cell_column = im_col.

    me->worksheet->set_cell(
      ip_row = im_row
      ip_column = im_col
      ip_value = im_value
      ip_style = im_style
    ).

  ENDMETHOD.

  METHOD set_worksheet_cols_auto_size.

    DATA: lo_column_dimension  TYPE REF TO
                                  zcl_excel_worksheet_columndime,
          lv_current_col_index TYPE i,
          lv_start_col_index   TYPE i,
          lv_end_col_index     TYPE i,
          lv_current_col       TYPE char2,
          lv_col_qty           TYPE i,
          lv_column_width      TYPE float.

    lv_start_col_index = zcl_excel_common=>convert_column2int(
      ip_column = im_start_col
    ).

    lv_end_col_index = zcl_excel_common=>convert_column2int(
      ip_column = im_end_col
    ).

    lv_col_qty = lv_end_col_index - lv_start_col_index + 1.

    DO lv_col_qty TIMES.

      lv_current_col_index = sy-index + lv_start_col_index - 1.
      lv_current_col = zcl_excel_common=>convert_column2alpha(
        ip_column = lv_current_col_index
      ).

      lo_column_dimension = me->worksheet->get_column_dimension(
        ip_column = lv_current_col
      ).

      lo_column_dimension->set_auto_size( ip_auto_size = abap_true ).

    ENDDO.

    me->worksheet->calculate_column_widths( ).

    " Now with the calculated width its possible to add a safety margin
    DO lv_col_qty TIMES.

      lv_current_col_index = sy-index + lv_start_col_index - 1.
      lv_current_col = zcl_excel_common=>convert_column2alpha(
        ip_column = lv_current_col_index
      ).

      lo_column_dimension = me->worksheet->get_column_dimension(
        ip_column = lv_current_col
      ).

      lv_column_width = lo_column_dimension->get_width( ) + 4.
      lo_column_dimension->set_auto_size( ip_auto_size = abap_false ).
      lo_column_dimension->set_width( ip_width = lv_column_width ).

    ENDDO.

  ENDMETHOD.

  METHOD create_fieldcatalog.

    " Important note: zcl_excel_common=>get_fieldcatalog sometimes do
    " not set correctly the position field, so it's better to set it
    " manually later
    re_fieldcat = zcl_excel_common=>get_fieldcatalog(
      ip_table = im_table
    ).

    DELETE re_fieldcat
      WHERE fieldname = 'EKORG'
         OR fieldname = 'EKGRP'
         OR fieldname = 'ESART'
         OR fieldname = 'DISPO'
         OR fieldname = 'DSNAM'
         OR fieldname = 'PABNUM'
         OR fieldname = 'PABPOS'
         OR fieldname = 'FIRM'
         OR fieldname = 'FABKL'
         OR fieldname = 'MAABC'
         OR fieldname = 'DAYS_ON_HAND'
         OR fieldname = 'CALC_SHIP_DATE'.

    LOOP AT re_fieldcat ASSIGNING FIELD-SYMBOL(<fcat_item>).
      <fcat_item>-position = sy-tabix.

      IF <fcat_item>-fieldname = 'ACTIONS'.
        <fcat_item>-scrtext_m = 'Actions'.
      ELSEIF <fcat_item>-fieldname = 'ACTUAL_DATE'.
        <fcat_item>-scrtext_m = 'Actual Delivery Date'.
      ELSEIF <fcat_item>-fieldname = 'ACTUAL_QTY'.
        <fcat_item>-scrtext_m = 'Actual Qty'.
      ELSEIF <fcat_item>-fieldname = 'PARTIAL_DATE'.
        <fcat_item>-scrtext_m = 'Partial Delivery Date'.
      ELSEIF <fcat_item>-fieldname = 'PARTIAL_QTY'.
        <fcat_item>-scrtext_m = 'Partial Qty'.
      ELSEIF <fcat_item>-fieldname = 'SUPPLIER_COMMENTS'.
        <fcat_item>-scrtext_m = 'Vendor Comments'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD apply_style_row.

    DATA: lv_current_col_index TYPE i,
          lv_start_col_index   TYPE i,
          lv_end_col_index     TYPE i,
          lv_current_col       TYPE char2,
          lv_col_qty           TYPE i.

    lv_start_col_index = zcl_excel_common=>convert_column2int(
      ip_column = im_start_col
    ).

    lv_end_col_index = zcl_excel_common=>convert_column2int(
      ip_column = im_end_col
    ).

    lv_col_qty = lv_end_col_index - lv_start_col_index + 1.

    DO lv_col_qty TIMES.

      lv_current_col_index = sy-index + lv_start_col_index - 1.
      lv_current_col = zcl_excel_common=>convert_column2alpha(
        ip_column = lv_current_col_index
      ).

*      TRY.

      me->worksheet->set_cell_style(
        ip_row = im_row
        ip_column = lv_current_col
        ip_style = im_style
      ).

*      CATCH ZCX_EXCEL INTO .
*
*      ENDTRY.


    ENDDO.

  ENDMETHOD.

ENDCLASS.

