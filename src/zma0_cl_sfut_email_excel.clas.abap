CLASS zma0_cl_sfut_email_excel DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,
      create_excel
        IMPORTING
          VALUE(delinq_table_raw) TYPE zma0_sfut_supplier_data_t
        EXPORTING
          ex_rawdata              TYPE solix_tab
          ex_bytecount            TYPE sood-objlen,
      download_frontend
        IMPORTING
          im_path TYPE string
          im_file TYPE string.

    CLASS-METHODS factory
      RETURNING VALUE(re_instance) TYPE REF TO zma0_cl_sfut_email_excel.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA excel TYPE REF TO zcl_excel.
    DATA worksheet TYPE REF TO zcl_excel_worksheet.
    DATA worksheet_range TYPE REF TO zcl_excel_range.

    DATA delinq_table TYPE STANDARD TABLE OF zma0_sfut_supplier_data.

    DATA action_columns TYPE string_table.

    DATA style_darkgreen_guid TYPE zexcel_cell_style.
    DATA style_yellow_guid TYPE zexcel_cell_style.
    DATA style_red_guid TYPE zexcel_cell_style.
    DATA style_date_guid TYPE zexcel_cell_style.
    DATA style_dt_yellow_guid TYPE zexcel_cell_style.
    DATA style_dt_red_guid TYPE zexcel_cell_style.
    DATA style_unprot_dt_guid TYPE zexcel_cell_style.
    DATA style_unprot_dt_yellow_guid TYPE zexcel_cell_style.
    DATA style_unprot_dt_red_guid TYPE zexcel_cell_style.
    DATA style_unprotected_guid TYPE zexcel_cell_style.
    DATA style_unprotected_yellow_guid TYPE zexcel_cell_style.
    DATA style_unprotected_red_guid TYPE zexcel_cell_style.


    CONSTANTS email_structure TYPE c LENGTH 23
      VALUE 'ZMA0_SFUT_SUPPLIER_DATA'.

    CONSTANTS not_shipped TYPE c LENGTH 31
      VALUE 'Should Have Shipped,But Has Not'.
    CONSTANTS shipping_today TYPE c LENGTH 24
      VALUE 'Should Be Shipping Today'.
    CONSTANTS shipped TYPE c LENGTH 28
      VALUE 'Should Have Shipped, And Has'.

    CONSTANTS table_initial_row TYPE i VALUE 1.
    CONSTANTS table_initial_col TYPE char1 VALUE 'A'.

    CONSTANTS worksheet_password TYPE c LENGTH 6
      VALUE 'secret'.

    CONSTANTS action_range_name TYPE c LENGTH 7
      VALUE 'Actions'.

    CONSTANTS dates_range_name TYPE c LENGTH 5
      VALUE 'Dates'.

    CONSTANTS reason_range_name TYPE c LENGTH 7
      VALUE 'Reasons'.

    CONSTANTS lang TYPE c LENGTH 2 VALUE 'EN'.

    METHODS: protect_current_worksheet,

      create_styles,

      create_range
        IMPORTING
                  im_tab_range_values TYPE string_table
                  im_range_name       TYPE string   " Do not use spaces
                  im_start_col        TYPE char2             DEFAULT 'A'
                  im_start_row        TYPE i                 DEFAULT 1
                  im_style            TYPE zexcel_cell_style OPTIONAL
                  im_data_type        TYPE abap_typekind     OPTIONAL
        EXPORTING
                  ex_end_row          TYPE i
        RAISING   zcx_excel,


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
                  im_style      TYPE zexcel_cell_style
        RAISING   zcx_excel,

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
                  im_style     TYPE zexcel_cell_style
        RAISING   zcx_excel,

      create_params_sheet
        RAISING zcx_excel,

      create_forms_sheet
        RAISING zcx_excel,

      create_attachment
        EXPORTING
          ex_rawdata   TYPE solix_tab
          ex_bytecount TYPE sood-objlen,

      get_email_components
        RETURNING VALUE(re_components)
                    TYPE abap_compdescr_tab,

      create_table
        RAISING zcx_excel,

      format_table_header
        RAISING zcx_excel,

      get_editable_columns
        RETURNING VALUE(re_columns) TYPE string_table,

      get_hidden_columns
        RETURNING VALUE(re_columns) TYPE string_table,

      format_table_body
        RAISING zcx_excel,

      get_action_values
        RETURNING VALUE(re_values) TYPE string_table,

      get_reason_values
        RETURNING
          VALUE(re_values) TYPE string_table,

      conv_table_idx_to_excel_col
        IMPORTING im_index      TYPE i
        RETURNING VALUE(re_col) TYPE zexcel_cell_column_alpha,

      get_ekorg
        RETURNING
          VALUE(re_ekorg) TYPE zma0_sfut_supplier_data-ekorg.

ENDCLASS.



CLASS zma0_cl_sfut_email_excel IMPLEMENTATION.
  METHOD constructor.

    " Create ABAP2XLSX instance
    me->excel = NEW zcl_excel( ).

    me->create_styles( ).

  ENDMETHOD.

  METHOD create_styles.
    DATA lo_style TYPE REF TO zcl_excel_style.
    CONSTANTS lc_date_format TYPE c LENGTH 10 VALUE 'dd/mm/yyyy'.

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

    " Date styles
    lo_style = excel->add_new_style( ).
    lo_style->number_format->format_code = lc_date_format.
    me->style_date_guid = lo_style->get_guid( ).

    lo_style = excel->add_new_style( ).
    lo_style->fill->filltype = zcl_excel_style_fill=>c_fill_solid.
    lo_style->fill->fgcolor-rgb = zcl_excel_style_color=>c_yellow.
    lo_style->number_format->format_code = lc_date_format.
    me->style_dt_yellow_guid = lo_style->get_guid( ).

    lo_style = excel->add_new_style( ).
    lo_style->fill->filltype = zcl_excel_style_fill=>c_fill_solid.
    lo_style->fill->fgcolor-rgb = zcl_excel_style_color=>c_red.
    lo_style->font->color-rgb = zcl_excel_style_color=>c_white.
    lo_style->number_format->format_code = lc_date_format.
    me->style_dt_red_guid = lo_style->get_guid( ).


    " Unprotected date styles
    lo_style = excel->add_new_style( ).
    lo_style->number_format->format_code = lc_date_format.
    lo_style->protection->locked =
      zcl_excel_style_protection=>c_protection_unlocked.
    me->style_unprot_dt_guid = lo_style->get_guid( ).

    lo_style = excel->add_new_style( ).
    lo_style->fill->filltype = zcl_excel_style_fill=>c_fill_solid.
    lo_style->fill->fgcolor-rgb = zcl_excel_style_color=>c_yellow.
    lo_style->number_format->format_code = lc_date_format.
    lo_style->protection->locked =
      zcl_excel_style_protection=>c_protection_unlocked.
    me->style_unprot_dt_yellow_guid = lo_style->get_guid( ).

    lo_style = excel->add_new_style( ).
    lo_style->fill->filltype = zcl_excel_style_fill=>c_fill_solid.
    lo_style->fill->fgcolor-rgb = zcl_excel_style_color=>c_red.
    lo_style->font->color-rgb = zcl_excel_style_color=>c_white.
    lo_style->number_format->format_code = lc_date_format.
    lo_style->protection->locked =
      zcl_excel_style_protection=>c_protection_unlocked.
    me->style_unprot_dt_red_guid = lo_style->get_guid( ).

  ENDMETHOD.

  METHOD create_excel.

    me->delinq_table[] = delinq_table_raw[].

    TRY.

        me->create_params_sheet( ).

        me->create_forms_sheet( ).

        me->create_attachment(
          IMPORTING
            ex_rawdata   = ex_rawdata
            ex_bytecount = ex_bytecount
        ).

      CATCH zcx_excel INTO DATA(l_error).

        MESSAGE l_error->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.

  METHOD get_ekorg.

    IF line_exists( me->delinq_table[ 1 ] ).
      re_ekorg = me->delinq_table[ 1 ]-ekorg.
    ENDIF.

  ENDMETHOD.

  METHOD create_params_sheet.
    DATA lv_date_str_range TYPE string_table.
    DATA lv_row TYPE i.
    DATA action_values TYPE string_table.

    CONSTANTS lc_worksheet_title TYPE c LENGTH 10 VALUE 'Parameters'.

    me->worksheet = me->excel->get_active_worksheet( ).
    me->worksheet->set_title( CONV #( lc_worksheet_title ) ).

    " Get range of ACTIONS
    action_values = me->get_action_values( ).
    " Create range of acceptance options
    me->create_range(
        EXPORTING
            im_range_name =
              CONV #( zma0_cl_sfut_email_excel=>action_range_name )
            im_tab_range_values = action_values
        IMPORTING
            ex_end_row = lv_row
    ).

    FREE action_values.

    " Create internal table for all dates
    me->create_date_str_table(
        EXPORTING
            im_range_days = 365
            im_start_date = sy-datum
        IMPORTING
            ex_tab_range_date = lv_date_str_range ).

    " Create range of dates
    me->create_range(
      EXPORTING
        im_range_name =
          CONV #( zma0_cl_sfut_email_excel=>dates_range_name )
        im_tab_range_values = lv_date_str_range
        im_start_row = lv_row + 1
        im_style = me->style_date_guid              " Apply date style
        im_data_type = cl_abap_typedescr=>typekind_num " Set as number
      IMPORTING
        ex_end_row = lv_row
    ).
    " Free alocated date table as it is not used anymore
    FREE lv_date_str_range.

    " Load all reasons
    DATA(lt_reasons) = me->get_reason_values( ).

    " Create range of reasons
    me->create_range(
        im_range_name =
          CONV #( zma0_cl_sfut_email_excel=>reason_range_name )
        im_tab_range_values = lt_reasons
        im_start_row = lv_row + 1
    ).
    " Free alocated date table as it is not used anymore
    FREE lt_reasons.

    me->protect_current_worksheet( ).

    me->worksheet->zif_excel_sheet_properties~zoomscale = 100.
    me->worksheet->zif_excel_sheet_properties~hidden = abap_true.


  ENDMETHOD.

  METHOD get_email_components.

    DATA : lr_struct_descr TYPE REF TO cl_abap_structdescr.

    lr_struct_descr ?= cl_abap_typedescr=>describe_by_name(
        zma0_cl_sfut_email_excel=>email_structure
    ).

    re_components = lr_struct_descr->components.

  ENDMETHOD.

  METHOD create_forms_sheet.

    CONSTANTS lc_worksheet_title TYPE c LENGTH 4 VALUE 'Form'.

    me->worksheet = me->excel->add_new_worksheet( ).
    me->worksheet->set_title( CONV #( lc_worksheet_title ) ).

    me->create_table( ).

    me->set_worksheet_cols_auto_size(
        im_start_col =
          CONV char2( zma0_cl_sfut_email_excel=>table_initial_col )
        im_end_col =
          CONV char2(
            zcl_excel_common=>convert_column2alpha(
              me->worksheet->get_highest_column( )
            )
          )
    ).

    me->protect_current_worksheet( ).

    " Set worksheet zoom
    me->worksheet->zif_excel_sheet_properties~zoomscale = 85.

  ENDMETHOD.

  METHOD create_table.
    DATA lv_is_table_settings TYPE zexcel_s_table_settings.
    DATA lv_delinq_fieldcat        TYPE zexcel_t_fieldcatalog.

    CONSTANTS lc_table_style TYPE c LENGTH 18
      VALUE 'TableStyleMedium13'.

    " Set table settings
    lv_is_table_settings-show_row_stripes = abap_true.
    lv_is_table_settings-top_left_column =
      zma0_cl_sfut_email_excel=>table_initial_col.
    lv_is_table_settings-top_left_row =
      zma0_cl_sfut_email_excel=>table_initial_row.
    lv_is_table_settings-table_style = lc_table_style.

    lv_delinq_fieldcat = create_fieldcatalog(
      im_table = me->delinq_table
    ).

    me->worksheet->bind_table(
      EXPORTING
        ip_table          = me->delinq_table
        it_field_catalog  = lv_delinq_fieldcat " Table binding fcat
        is_table_settings = lv_is_table_settings " Excel table settings
    ).

    " Format header columns and hide specified columns
    me->format_table_header( ).

    me->format_table_body( ).

  ENDMETHOD.

  METHOD get_editable_columns.
    re_columns = VALUE #(
      ( |ACTION| )
      ( |REASON| )
      ( |ACTUAL_DATE| )
      ( |ACTUAL_QTY| )
      ( |PARTIAL_DATE| )
      ( |PARTIAL_QTY| )
      ( |SUPPLIER_COMMENT| )
    ).
  ENDMETHOD.

  METHOD get_hidden_columns.
    re_columns = VALUE #(
      ( |EKORG| )
      ( |WERKS| )
      ( |PLANT_NAME| )
      ( |EKGRP| )
      ( |DISPO| )
      ( |ETENR| )
    ).
  ENDMETHOD.

  METHOD format_table_body.
    DATA lv_tab_index TYPE sy-tabix.
    DATA lo_data_validation TYPE REF TO zcl_excel_data_validation.
    DATA lv_style_guid TYPE zexcel_cell_style.
    DATA lv_editable_style_color_guid TYPE zexcel_cell_style.
    DATA lv_date_style_guid TYPE zexcel_cell_style.
    DATA lv_unprot_date_style_guid TYPE zexcel_cell_style.
    DATA lv_components TYPE abap_compdescr_tab.
    DATA lv_current_col TYPE zexcel_cell_column_alpha.

    " Special formatted fields
    CONSTANTS lc_action TYPE c LENGTH 6 VALUE 'ACTION'.
    CONSTANTS lc_reason TYPE c LENGTH 6 VALUE 'REASON'.
    CONSTANTS lc_actual_date TYPE c LENGTH 11 VALUE 'ACTUAL_DATE'.
    CONSTANTS lc_partial_date TYPE c LENGTH 12 VALUE 'PARTIAL_DATE'.
    CONSTANTS lc_actual_qty TYPE c LENGTH 10 VALUE 'ACTUAL_QTY'.
    CONSTANTS lc_partial_qty TYPE c LENGTH 11 VALUE 'PARTIAL_QTY'.
    CONSTANTS lc_supp_comment TYPE c LENGTH 16 VALUE 'SUPPLIER_COMMENT'.
    CONSTANTS lc_calc_ship_date TYPE c LENGTH 16 VALUE 'CALC_SHIP_DATE'.

    " Range placeholder names
    CONSTANTS lc_select_date_text TYPE c LENGTH 11
      VALUE 'Select date'.
    CONSTANTS lc_select_action_text TYPE c LENGTH 13
      VALUE 'Select action'.
    CONSTANTS lc_select_reason_text TYPE c LENGTH 13
      VALUE 'Select reason'.

    lv_components = me->get_email_components( ).

    " For each line in the table
    LOOP AT me->delinq_table ASSIGNING FIELD-SYMBOL(<delinq_item>).
      " Excel table index
      lv_tab_index =
        sy-tabix + zma0_cl_sfut_email_excel=>table_initial_row.

      " Conditional styling on status
      CLEAR lv_style_guid.
      lv_editable_style_color_guid = me->style_unprotected_guid.
      lv_date_style_guid = me->style_date_guid.
      lv_unprot_date_style_guid = me->style_unprot_dt_guid.

      IF <delinq_item>-ship_status =
        zma0_cl_sfut_email_excel=>not_shipped.

        lv_style_guid = me->style_red_guid.
        lv_editable_style_color_guid = me->style_unprotected_red_guid.
        lv_date_style_guid = me->style_dt_red_guid.
        lv_unprot_date_style_guid = me->style_unprot_dt_red_guid.

      ELSEIF <delinq_item>-ship_status =
            zma0_cl_sfut_email_excel=>shipping_today
          OR <delinq_item>-ship_status =
            zma0_cl_sfut_email_excel=>shipped.

        lv_style_guid = me->style_yellow_guid.
        lv_editable_style_color_guid =
          me->style_unprotected_yellow_guid.
        lv_date_style_guid = me->style_dt_yellow_guid.
        lv_unprot_date_style_guid = me->style_unprot_dt_yellow_guid.

      ENDIF.


      LOOP AT lv_components ASSIGNING FIELD-SYMBOL(<body_component>).
        lv_current_col = conv_table_idx_to_excel_col( sy-tabix ).

        CASE <body_component>-name.
          WHEN lc_action.

            me->create_dropdown(
              EXPORTING
                im_range_name =
                  CONV #( zma0_cl_sfut_email_excel=>action_range_name )
                im_value      = CONV #( lc_select_action_text )
                im_row        = lv_tab_index
                im_col        = lv_current_col
                im_style         = lv_editable_style_color_guid
            ).

          WHEN lc_reason.

            me->create_dropdown(
              EXPORTING
                im_range_name =
                  CONV #( zma0_cl_sfut_email_excel=>reason_range_name )
                im_value      = CONV #( lc_select_reason_text )
                im_row        = lv_tab_index
                im_col        = lv_current_col
                im_style         = lv_editable_style_color_guid
            ).

          WHEN lc_actual_date OR lc_partial_date.

            me->create_dropdown(
              EXPORTING
                im_range_name =
                  CONV #( zma0_cl_sfut_email_excel=>dates_range_name )
                im_value      = CONV #( lc_select_date_text )
                im_row        = lv_tab_index
                im_col        = lv_current_col
                im_style         = lv_unprot_date_style_guid
            ).

          WHEN lc_actual_qty OR lc_partial_qty.

            lo_data_validation =
              me->worksheet->add_new_data_validation( ).
            lo_data_validation->type =
              zcl_excel_data_validation=>c_type_whole.
            lo_data_validation->cell_row = lv_tab_index.
            lo_data_validation->cell_column = lv_current_col.

            me->worksheet->set_cell_style(
              EXPORTING
                ip_column = lv_current_col
                ip_row    = lv_tab_index
                ip_style  = lv_editable_style_color_guid
            ).

          WHEN lc_supp_comment.

            me->worksheet->set_cell_style(
              EXPORTING
                ip_column = lv_current_col
                ip_row    = lv_tab_index
                ip_style  = lv_editable_style_color_guid
            ).

          WHEN lc_calc_ship_date.

            me->worksheet->set_cell_style(
              EXPORTING
                ip_column = lv_current_col
                ip_row    = lv_tab_index
                ip_style  = lv_date_style_guid
            ).

          WHEN OTHERS.

            IF lv_style_guid IS NOT INITIAL.

              me->worksheet->set_cell_style(
                EXPORTING
                  ip_column = lv_current_col
                  ip_row    = lv_tab_index
                  ip_style  = lv_style_guid
              ).

            ENDIF.

        ENDCASE.
      ENDLOOP.


    ENDLOOP.

  ENDMETHOD.

  METHOD conv_table_idx_to_excel_col.
    DATA initial_idx TYPE zexcel_cell_column.
    DATA final_idx TYPE i.

    initial_idx = zcl_excel_common=>convert_column2int(
      CONV #( zma0_cl_sfut_email_excel=>table_initial_col )
    ).

    final_idx = im_index + initial_idx - 1.

    re_col = zcl_excel_common=>convert_column2alpha( final_idx ).

  ENDMETHOD.

  METHOD format_table_header.
    DATA editable_columns TYPE string_table.
    DATA hidden_columns TYPE string_table.
    DATA lv_column TYPE zexcel_cell_column_alpha.
    DATA lv_col_dim TYPE REF TO zcl_excel_worksheet_columndime.
    DATA lv_components TYPE abap_compdescr_tab.


    lv_components = me->get_email_components( ).

    " Format header columns and hide specified columns
    LOOP AT lv_components ASSIGNING FIELD-SYMBOL(<component>).

      lv_column = conv_table_idx_to_excel_col( sy-tabix ).

      editable_columns = me->get_editable_columns( ).
      hidden_columns = me->get_hidden_columns( ).

      IF line_exists(
        editable_columns[ table_line = <component>-name ]
      ).

        me->worksheet->set_cell_style(
          EXPORTING
            ip_column = lv_column
            ip_row    = zma0_cl_sfut_email_excel=>table_initial_row
            ip_style  = style_darkgreen_guid
        ).
      ELSEIF line_exists(
        hidden_columns[ table_line = <component>-name ]
      ).

        lv_col_dim = me->worksheet->get_column_dimension( lv_column ).
        lv_col_dim->set_visible( abap_false ).

      ENDIF.
    ENDLOOP.



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
    cl_gui_frontend_services=>gui_download(
      EXPORTING bin_filesize = lv_i_bytecount
                filename     = lv_filename
                filetype     = 'BIN'
      CHANGING  data_tab     = lv_t_rawdata ).
  ENDMETHOD.

  METHOD protect_current_worksheet.
    IF me->worksheet IS NOT INITIAL.
      me->worksheet->zif_excel_sheet_protection~protected  =
        zif_excel_sheet_protection=>c_protected.
      worksheet->zif_excel_sheet_protection~password   =
        zcl_excel_common=>encrypt_password(
          CONV #( zma0_cl_sfut_email_excel=>worksheet_password )
        ).
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

        IF im_data_type IS INITIAL.

          me->worksheet->set_cell(
            ip_row = lv_start_row
            ip_column = lv_start_col
            ip_value = <range_value>
          ).

        ELSE.

          me->worksheet->set_cell(
            ip_row = lv_start_row
            ip_column = lv_start_col
            ip_value = <range_value>
            ip_data_type = CONV zexcel_cell_data_type( im_data_type )
          ).

        ENDIF.

        IF im_style IS NOT INITIAL.

          me->worksheet->set_cell_style(
            EXPORTING
              ip_row    = lv_start_row
              ip_column = lv_start_col
              ip_style  = im_style
          ).

        ENDIF.

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

      lv_str_date = zcl_excel_common=>date_to_excel_string(
        ip_value = lv_date_iterator
      ).

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

    LOOP AT re_fieldcat ASSIGNING FIELD-SYMBOL(<fcat_item>).
      " Assigning position to ensure incremental pattern
      <fcat_item>-position = sy-tabix.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_action_values.

    CONSTANTS lc_action_domain TYPE dd07l-domname
        VALUE 'ZMA0_SFUT_ACTION'.

    DATA: lt_domain TYPE TABLE OF dd07v.

    " Get texts from the action domain
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = lc_action_domain
        text           = abap_true
        langu          = CONV ddlanguage(
                          zma0_cl_sfut_email_excel=>lang )
      TABLES
        dd07v_tab      = lt_domain
      EXCEPTIONS
        wrong_textflag = 1
        error_message  = 2
        OTHERS         = 3.

    IF sy-subrc IS INITIAL.
      LOOP AT lt_domain ASSIGNING FIELD-SYMBOL(<domain_item>).
        APPEND <domain_item>-ddtext TO re_values.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD get_reason_values.
    DATA lv_ekorg TYPE zma0_sfut_supplier_data-ekorg.

    lv_ekorg = me->get_ekorg( ).

    SELECT
      ekorg,
      langu,
      reason
      FROM zma0_sfut_reas
      INTO TABLE @DATA(lt_reasons)
      WHERE ekorg = @lv_ekorg
        AND langu = @sy-langu.

    IF sy-subrc IS INITIAL.
      LOOP AT lt_reasons ASSIGNING FIELD-SYMBOL(<reason_item>).
        APPEND <reason_item>-reason TO re_values.
      ENDLOOP.
    ENDIF.

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

      me->worksheet->set_cell_style(
        ip_row = im_row
        ip_column = lv_current_col
        ip_style = im_style
      ).

    ENDDO.

  ENDMETHOD.

  METHOD factory.
    re_instance = NEW #( ).
  ENDMETHOD.

ENDCLASS.

