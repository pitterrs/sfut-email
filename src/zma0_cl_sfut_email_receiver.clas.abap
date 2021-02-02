CLASS zma0_cl_sfut_email_receiver DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES tt_ignored_columns TYPE TABLE OF i WITH DEFAULT KEY.

    TYPES tt_file_layout TYPE TABLE OF zma0_sfut_supplier_data
    WITH NON-UNIQUE DEFAULT KEY.

    METHODS constructor
      IMPORTING VALUE(im_file_path) TYPE string
      RAISING   zcx_excel.

    CLASS-METHODS factory
      IMPORTING VALUE(im_file_path) TYPE string
      RETURNING VALUE(re_instance)
                  TYPE REF TO zma0_cl_sfut_email_receiver
      RAISING   zcx_excel.

    METHODS get_file_data
      RETURNING VALUE(re_file_data) TYPE tt_file_layout.

  PRIVATE SECTION.

    DATA worksheet TYPE REF TO zcl_excel_worksheet.

    DATA components TYPE abap_compdescr_tab.

    DATA ignored_columns TYPE tt_ignored_columns.

    CONSTANTS email_structure TYPE c LENGTH 23
        VALUE 'ZMA0_SFUT_SUPPLIER_DATA'.

    CONSTANTS lang TYPE c LENGTH 2 VALUE 'EN'.

    METHODS get_email_components
      RETURNING VALUE(re_components)
                  TYPE abap_compdescr_tab.

    METHODS get_cell_value
      IMPORTING VALUE(im_row)    TYPE i
                VALUE(im_column) TYPE zexcel_cell_column
      RETURNING VALUE(re_value)  TYPE zexcel_cell_value.

    METHODS set_field_value
      IMPORTING VALUE(im_column)    TYPE zexcel_cell_column
                VALUE(im_value)     TYPE zexcel_cell_value
      CHANGING  VALUE(ch_work_area) TYPE zma0_sfut_supplier_data.

    METHODS convert_action_text
      IMPORTING VALUE(im_value)  TYPE zexcel_cell_value
      RETURNING VALUE(re_action) TYPE zma0_sfut_action.

    METHODS convert_date
      IMPORTING VALUE(im_value) TYPE zexcel_cell_value
      RETURNING VALUE(re_date)  TYPE datum.

ENDCLASS.

CLASS zma0_cl_sfut_email_receiver IMPLEMENTATION.

  METHOD constructor.

    DATA(lr_reader) = NEW zcl_excel_reader_2007(  ).

    DATA(lr_excel) = lr_reader->zif_excel_reader~load_file(
        im_file_path
    ).

    CHECK lr_excel IS BOUND.

    me->worksheet = lr_excel->get_active_worksheet( ).
    me->components = me->get_email_components( ).

  ENDMETHOD.

  METHOD factory.
    re_instance = NEW #( im_file_path ).
  ENDMETHOD.

  METHOD get_file_data.

    DATA lw_supplier_data TYPE zma0_sfut_supplier_data.

    DATA lv_row TYPE i VALUE 2. " Start from 2, ignore the header
    DATA lv_column TYPE zexcel_cell_column VALUE 1.

    DATA(lv_highest_column) = me->worksheet->get_highest_column( ).
    DATA(lv_highest_row) = me->worksheet->get_highest_row( ).

    WHILE lv_row <= lv_highest_row. " Vertical loop (rows)

      WHILE lv_column <= lv_highest_column. " Horizontal loop (columns)

        " Set value on the e-mail receiving structure
        me->set_field_value(
          EXPORTING
            im_column    = lv_column
            im_value     = me->get_cell_value(
                im_row    = lv_row
                im_column = lv_column
            )
          CHANGING
            ch_work_area = lw_supplier_data
        ).

        ADD 1 TO lv_column. " Go to the next column

      ENDWHILE.

      " Add excel register to the returning SAP internal table
      INSERT lw_supplier_data INTO TABLE re_file_data .
      CLEAR lw_supplier_data.

      " Go to the next row
      lv_column = 1.
      ADD 1 TO lv_row.

    ENDWHILE.

  ENDMETHOD.

  METHOD get_cell_value.

    " Get the column cell
    DATA(lv_column_letter) = zcl_excel_common=>convert_column2alpha(
        im_column
    ).

    " Get cell value from the row/column
    me->worksheet->get_cell(
      EXPORTING
        ip_column = lv_column_letter
        ip_row    = im_row
      IMPORTING
        ep_value = re_value
    ).

  ENDMETHOD.

  METHOD get_email_components.

    DATA : lr_struct_descr TYPE REF TO cl_abap_structdescr.

    lr_struct_descr ?= cl_abap_typedescr=>describe_by_name(
        zma0_cl_sfut_email_receiver=>email_structure
    ).

    re_components = lr_struct_descr->components.

  ENDMETHOD.

  METHOD set_field_value.

    CONSTANTS lc_action TYPE c LENGTH 19
        VALUE 'CH_WORK_AREA-ACTION'.
    CONSTANTS lc_actual_date TYPE c LENGTH 24
        VALUE 'CH_WORK_AREA-ACTUAL_DATE'.
    CONSTANTS lc_partial_date TYPE c LENGTH 25
        VALUE 'CH_WORK_AREA-PARTIAL_DATE'.

    DATA(lv_field) =
        |CH_WORK_AREA-{ me->components[ im_column ]-name }|.

    ASSIGN (lv_field) TO FIELD-SYMBOL(<field>).

    IF sy-subrc IS INITIAL
    AND <field> IS ASSIGNED.
      <field> = COND zexcel_cell_value(
        WHEN lv_field = lc_action " Handle action field
        THEN me->convert_action_text( im_value )
        WHEN lv_field = lc_actual_date
          OR lv_field = lc_partial_date
        THEN me->convert_date( im_value )
        ELSE im_value
      ).
    ENDIF.

  ENDMETHOD.

  METHOD convert_action_text.

    CONSTANTS lc_action_domain TYPE dd07l-domname
        VALUE 'ZMA0_SFUT_ACTION'.

    DATA: lt_domain TYPE TABLE OF dd07v.

    " Get texts from the action domain
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = lc_action_domain
        text           = abap_true
        langu          = CONV ddlanguage(
                          zma0_cl_sfut_email_receiver=>lang )
      TABLES
        dd07v_tab      = lt_domain
      EXCEPTIONS
        wrong_textflag = 1
        error_message  = 2
        OTHERS         = 3.

    IF sy-subrc IS INITIAL.
      re_action = COND #(
        WHEN line_exists( lt_domain[ ddtext = im_value ] )
        THEN lt_domain[ ddtext = im_value ]-domvalue_l
        ELSE space
      ).
    ENDIF.

  ENDMETHOD.

  METHOD convert_date.

    CONSTANTS lc_empty_date TYPE c LENGTH 11 VALUE 'Select date'.

    re_date = COND #(
        WHEN im_value = lc_empty_date
          OR im_value IS INITIAL
        THEN space
        ELSE zcl_excel_common=>excel_string_to_date( im_value )
    ).

  ENDMETHOD.

ENDCLASS.
