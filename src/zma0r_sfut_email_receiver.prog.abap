************************************************************************
*                                                                      *
*                           Modification Log                           *
*                                                                      *
* Program Name: zma0r_sfut_email_receiver                              *
*                                                                      *
* Author: Nicholas Checan & Peter Nascimento &                         *
*         Eloisa Matthiesen & Eduardo Weber                            *
* Date Written: January 26th, 2021                                     *
* Request #: CAGK9C1ZFV                                                *
* Requested by: Carlos Vargas (VargasCarlosA@JohnDeere.com)            *
* Description: This program intends to receive the excel file related  *
* to the supplier follow up process. The same will be sent through the *
* /DEERE/ZMDELINQH. The class ZMA0_CL_SFUT_EMAIL_RECEIVER will be the  *
* one responsible for the e-mail attachment processing.                *
*                                                                      *
* Mod date  Programmer    Reference   Description                      *
*----------------------------------------------------------------------*
*
************************************************************************

REPORT zma0r_sfut_email_receiver.

**********************************************************************
* Classes
**********************************************************************

CLASS lcx_update_error DEFINITION INHERITING FROM cx_static_check.

  PUBLIC SECTION.

    METHODS constructor.

ENDCLASS.

CLASS lcx_update_error IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_database DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_schedule_line,
             ebeln TYPE eket-ebeln,
             ebelp TYPE eket-ebelp,
             etenr TYPE eket-etenr,
           END OF ty_schedule_line.

    TYPES tt_schedule_line TYPE SORTED TABLE OF ty_schedule_line
    WITH UNIQUE KEY primary_key COMPONENTS ebeln ebelp etenr.

    TYPES tt_file_layout TYPE TABLE OF zma0_sfut_supplier_data
    WITH NON-UNIQUE DEFAULT KEY.

    METHODS constructor
      IMPORTING VALUE(im_file_data) TYPE tt_file_layout.

    METHODS get_next_schedule_line
      IMPORTING VALUE(im_ebeln) TYPE eket-ebeln
                VALUE(im_ebelp) TYPE eket-ebelp
      RETURNING VALUE(re_next)  TYPE eket-etenr.

  PRIVATE SECTION.

    DATA last_schedule_lines TYPE tt_schedule_line.

    METHODS retrieve_last_schedule_lines
      IMPORTING VALUE(im_file_data) TYPE tt_file_layout
      RETURNING VALUE(re_result)    TYPE lcl_database=>tt_schedule_line.

ENDCLASS.

CLASS lcl_database IMPLEMENTATION.

  METHOD constructor.

    " Select max on the ETENR column to get the last items for usage on
    " the schedule lines creation procedure
    me->last_schedule_lines = me->retrieve_last_schedule_lines(
        im_file_data " Pass the file data to filter the information
    ).

  ENDMETHOD.

  METHOD retrieve_last_schedule_lines.

    DATA lt_order_items TYPE SORTED TABLE OF zma0_sfut_supplier_data
    WITH NON-UNIQUE KEY primary_key COMPONENTS ebeln ebelp.

    " Prepare data for the selection operation
    lt_order_items = im_file_data.
    DELETE ADJACENT DUPLICATES FROM lt_order_items
    USING KEY primary_key.

    CHECK lt_order_items IS NOT INITIAL.

    " Get all schedule lines based on the filtered purchasing orders
    SELECT ebeln,
           ebelp,
           etenr
    FROM eket
    INTO TABLE @DATA(lt_schedule_lines)
    FOR ALL ENTRIES IN @lt_order_items
    WHERE ebeln = @lt_order_items-ebeln
    AND ebelp = @lt_order_items-ebelp.

    CHECK sy-subrc IS INITIAL.

    " Remove duplicates considering only the last schedule lines
    SORT lt_schedule_lines
    BY ebeln ebelp ASCENDING etenr DESCENDING.

    DELETE ADJACENT DUPLICATES FROM lt_schedule_lines
    COMPARING ebeln ebelp.

    re_result = lt_schedule_lines.

  ENDMETHOD.

  METHOD get_next_schedule_line.

    re_next = COND #(
        WHEN line_exists( me->last_schedule_lines[
            ebeln = im_ebeln
            ebelp = im_ebelp
        ] )
        THEN me->last_schedule_lines[
            ebeln = im_ebeln
            ebelp = im_ebelp
        ]-etenr + 1
        ELSE 1
    ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_main DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_log,
             fline  TYPE i,
             ekorg  TYPE ekorg,
             ekgrp  TYPE ekgrp,
             lifnr  TYPE lifnr,
             werks  TYPE werks_d,
             status TYPE icon_d.
            INCLUDE TYPE bapiret2.
    TYPES END OF ty_log.

    TYPES tt_log TYPE TABLE OF ty_log
    WITH NON-UNIQUE DEFAULT KEY.

    TYPES tt_material_memo TYPE TABLE OF bapi_mltx_ga
    WITH NON-UNIQUE DEFAULT KEY.

    TYPES tt_file_layout TYPE TABLE OF zma0_sfut_supplier_data
    WITH NON-UNIQUE DEFAULT KEY.

    TYPES tt_column_name TYPE TABLE OF lvc_fname
    WITH NON-UNIQUE DEFAULT KEY.

    METHODS constructor
      IMPORTING VALUE(im_filepath) TYPE zexcel_export_dir.

    CLASS-METHODS run
      IMPORTING VALUE(im_filepath) TYPE zexcel_export_dir
      RETURNING VALUE(re_instance) TYPE REF TO lcl_main.

    METHODS log_display.

  PRIVATE SECTION.

    DATA database TYPE REF TO lcl_database.

    DATA file_data TYPE tt_file_layout.

    DATA log TYPE tt_log.

    METHODS update_operation
      IMPORTING VALUE(im_file_data) TYPE zma0_sfut_supplier_data
      RETURNING VALUE(re_log)       TYPE lcl_main=>ty_log
      RAISING   lcx_update_error.

    METHODS maintain_schedule_line
      IMPORTING VALUE(im_new)       TYPE abap_bool
                VALUE(im_file_data) TYPE zma0_sfut_supplier_data
                VALUE(im_file_line) TYPE syst-tabix
      RAISING   lcx_update_error.

    METHODS update_material_memo
      IMPORTING VALUE(im_file_data) TYPE zma0_sfut_supplier_data
                VALUE(im_file_line) TYPE syst-tabix
      RAISING   lcx_update_error.

    METHODS get_material_memo
      IMPORTING VALUE(im_matnr)  TYPE zma0_sfut_supplier_data-matnr
                VALUE(im_werks)  TYPE zma0_sfut_supplier_data-werks
      RETURNING VALUE(re_result) TYPE t_bapi_mltx.

    METHODS add_supplier_comment
      IMPORTING VALUE(im_matnr)         TYPE matnr
                VALUE(im_werks)         TYPE werks_d
                VALUE(im_comment)       TYPE zma0_sfut_comment
      CHANGING  VALUE(ch_material_memo) TYPE t_bapi_mltx.

    METHODS save_material_data
      IMPORTING VALUE(im_file_data)     TYPE zma0_sfut_supplier_data
                VALUE(im_file_line)     TYPE syst-tabix
                VALUE(im_material_memo) TYPE t_bapi_mltx
      EXPORTING VALUE(ex_bapiret)       TYPE bapiret2_t
      RAISING   lcx_update_error.

    METHODS add_log_data
      IMPORTING VALUE(im_file_line) TYPE syst-tabix
                VALUE(im_file_data) TYPE zma0_sfut_supplier_data
                VALUE(im_bapiret)   TYPE bapiret2_t.

    METHODS import_file_data
      IMPORTING VALUE(im_filepath) TYPE any
      RETURNING VALUE(r_result)    TYPE lcl_main=>tt_file_layout.

    METHODS handle_report_columns
      CHANGING VALUE(ch_alv) TYPE REF TO cl_salv_table.

    METHODS get_hidden_columns
      RETURNING VALUE(re_result) TYPE lcl_main=>tt_column_name.

    METHODS convert_date
      IMPORTING VALUE(im_date)
                  TYPE zma0_sfut_supplier_data-actual_date
      RETURNING VALUE(re_result) TYPE eeind.

    METHODS add_system_log
      IMPORTING VALUE(im_syst)    TYPE syst
      CHANGING  VALUE(ch_bapiret) TYPE bapiret2_t.

    METHODS commit.

    METHODS roll_back.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD constructor.

    DATA lv_errors_found TYPE abap_bool VALUE abap_false.
    CONSTANTS lc_info TYPE c LENGTH 1 VALUE 'I'.
    CONSTANTS lc_error TYPE c LENGTH 1 VALUE 'E'.

    " Retrieve file data from the informed file path
    me->file_data = me->import_file_data( CONV string( im_filepath ) ).

    " Select operations and other database related procedures
    me->database = NEW #( me->file_data ).

    " Update the SAP database with data from the Excel File
    LOOP AT me->file_data INTO DATA(lw_file_data).
      TRY.
          me->update_operation( lw_file_data ).
        CATCH lcx_update_error.
          lv_errors_found = abap_true.
          CONTINUE.
      ENDTRY.
    ENDLOOP.

    IF lv_errors_found IS INITIAL.
      me->commit( ). " Consolidate database changes
    ELSE.
      me->roll_back( ). " Revert back the changes
      MESSAGE text-008 TYPE lc_info DISPLAY LIKE lc_error.
    ENDIF.

    " Display an ALV report with the data from the previous operations
    me->log_display( ).

  ENDMETHOD.

  METHOD run.
    re_instance = NEW #( im_filepath ).
  ENDMETHOD.

  METHOD update_operation.

    DATA(lv_file_line) = sy-tabix.

    CONSTANTS lc_proposed TYPE zma0_sfut_action VALUE '3'.

    " Check if a new schedule date and quantity has been proposed...
    IF im_file_data-action = lc_proposed.

      " Update the schedule line with the new proposed date and qty
      IF im_file_data-actual_date IS NOT INITIAL
      AND im_file_data-actual_qty IS NOT INITIAL.
        me->maintain_schedule_line(
            EXPORTING
                im_new       = abap_false " Update schedule line
                im_file_data = im_file_data
                im_file_line = lv_file_line
        ).
      ENDIF.

      " Create a new schedule line with the partial date and qty
      IF im_file_data-partial_date IS NOT INITIAL
      AND im_file_data-partial_qty IS NOT INITIAL.
        me->maintain_schedule_line(
            EXPORTING
                im_new       = abap_true " Create schedule line
                im_file_data = im_file_data
                im_file_line = lv_file_line
        ).
      ENDIF.

    ENDIF.

    " Update the material text at the MD04 transaction
    IF im_file_data-supplier_comment IS NOT INITIAL.
      me->update_material_memo(
            im_file_data = im_file_data
            im_file_line = lv_file_line
      ).
    ENDIF.

  ENDMETHOD.

  METHOD maintain_schedule_line.

    DATA lt_schedule TYPE TABLE OF bapimeoutschedule.
    DATA lt_schedulex TYPE TABLE OF bapimeoutschedulex.
    DATA lt_return TYPE TABLE OF bapiret2.

    DATA lv_schedule_item TYPE etenr.
    DATA lv_delivery_date TYPE eeind.
    DATA lv_quantity TYPE etmen.

    CONSTANTS lc_error TYPE c LENGTH 1 VALUE 'E'.

    " Check if the schedule line is going to be created or updated
    IF im_new IS INITIAL.
      lv_schedule_item = im_file_data-etenr.
      lv_delivery_date = me->convert_date( im_file_data-actual_date ).
      lv_quantity      = im_file_data-actual_qty.
    ELSE.
      " Get next schedule line item per purchasing document and item
      lv_schedule_item  = me->database->get_next_schedule_line(
        im_ebeln = im_file_data-ebeln
        im_ebelp = im_file_data-ebelp
      ).
      lv_delivery_date = me->convert_date( im_file_data-partial_date ).
      lv_quantity      = im_file_data-partial_qty.
    ENDIF.

    " Delivery Schedule Line Data Outline Agreement
    APPEND VALUE bapimeoutschedule(
        item_no       = im_file_data-ebelp
        sched_line    = lv_schedule_item
        delivery_date = lv_delivery_date
        quantity      = lv_quantity
    ) TO lt_schedule.

    " Schedule Line Data - Change Parameter - Outline Agreement
    APPEND VALUE bapimeoutschedulex(
        item_no       = im_file_data-ebelp
        sched_line    = lv_schedule_item
        delivery_date = abap_true
        quantity      = abap_true
    ) TO lt_schedulex.

    " Change or create the schedule line information
    CALL FUNCTION 'BAPI_SCHEDULE_MAINTAIN'
      EXPORTING
        purchasingdocument = im_file_data-ebeln
      TABLES
        schedule           = lt_schedule
        schedulex          = lt_schedulex
        return             = lt_return
      EXCEPTIONS
        error_message      = 1
        OTHERS             = 2.

    " Add error message to the BAPI return so the program can log it
    IF sy-subrc IS NOT INITIAL.
      me->add_system_log(
        EXPORTING
            im_syst    = syst
        CHANGING
            ch_bapiret = lt_return
        ).
    ENDIF.

    " Add BAPI return messages internal table into the log
    IF lt_return IS NOT INITIAL.
      me->add_log_data( " Set operation log
          im_file_line = im_file_line
          im_file_data = im_file_data
          im_bapiret   = lt_return
      ).
    ENDIF.

    " Stop line processing and go to the next update operation
    IF line_exists( lt_return[ type = lc_error ] ).
      RAISE EXCEPTION TYPE lcx_update_error.
    ENDIF.

  ENDMETHOD.

  METHOD update_material_memo.

    " Get material memo text from the MD04 transaction
    DATA(lt_material_memo) = me->get_material_memo(
        im_matnr = im_file_data-matnr
        im_werks = im_file_data-werks
    ).

    " Update the material memo internal table with the supplier comment
    me->add_supplier_comment(
        EXPORTING
            im_matnr         = im_file_data-matnr
            im_werks         = im_file_data-werks
            im_comment       = im_file_data-supplier_comment
        CHANGING
            ch_material_memo = lt_material_memo
    ).

    " Update the material memo text
    me->save_material_data(
        im_file_data     = im_file_data
        im_file_line     = im_file_line
        im_material_memo = lt_material_memo
    ).

  ENDMETHOD.

  METHOD get_material_memo.

    DATA lt_current_material_memo TYPE lcl_main=>tt_material_memo.

    " Get the current material memo text data (from the MD04)
    CALL FUNCTION 'BAPI_MATERIAL_GETALL'
      EXPORTING
        material      = im_matnr
        plant         = im_werks
      TABLES
        materialtext  = lt_current_material_memo
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.

    CHECK sy-subrc IS INITIAL.

    " Convert internal table type for the update operation
    re_result = VALUE t_bapi_mltx(
        FOR lw_current_line IN lt_current_material_memo (
            applobject = lw_current_line-applobject
            text_name  = lw_current_line-text_name
            text_id    = lw_current_line-text_id
            langu      = lw_current_line-langu
            langu_iso  = lw_current_line-langu_iso
            format_col = lw_current_line-format_col
            text_line  = lw_current_line-text_line
        )
    ).

  ENDMETHOD.

  METHOD add_supplier_comment.

    CONSTANTS lc_application_object TYPE tdobject VALUE `MDTXT`.
    CONSTANTS lc_text_id TYPE tdid VALUE `LTXT`.
    CONSTANTS lc_line_len TYPE i VALUE 132. " Max size of the TDLINE

    DATA lt_comment_lines TYPE trtexts.

    CALL FUNCTION 'TR_SPLIT_TEXT'
      EXPORTING
        iv_text       = im_comment
        iv_len        = lc_line_len
      IMPORTING
        et_lines      = lt_comment_lines
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.

    CHECK sy-subrc IS INITIAL.

    " Add supplier comment header data
    APPEND VALUE bapi_mltx(
        applobject = lc_application_object
        text_name  = |{ im_matnr } { im_werks }|
        text_id    = lc_text_id
        langu      = sy-langu
        langu_iso  = sy-langu
        format_col = '*'
        text_line  = |{ sy-datum } - { sy-uname } - { text-003 }|
    ) TO ch_material_memo.

    " Add all comment lines to the Material Memo at the MD04 transaction
    LOOP AT lt_comment_lines INTO DATA(lv_comment_line).

      DATA(lv_column_format) = COND tdformat(
        WHEN sy-tabix = 1
        THEN '*' ELSE space
      ).

      " Add supplier comment into the material memo
      APPEND VALUE bapi_mltx(
          applobject = lc_application_object
          text_name  = |{ im_matnr } { im_werks }|
          text_id    = lc_text_id
          langu      = sy-langu
          langu_iso  = sy-langu
          format_col = lv_column_format
          text_line  = lv_comment_line
      ) TO ch_material_memo.

    ENDLOOP.

  ENDMETHOD.

  METHOD save_material_data.

    DATA lw_headdata TYPE bapimathead.
    DATA lw_plantdata TYPE bapi_marc.
    DATA lw_plantdatax TYPE bapi_marcx.
    DATA lw_return TYPE bapiret2.
    CONSTANTS lc_error TYPE c LENGTH 1 VALUE 'E'.

    CHECK im_material_memo IS NOT INITIAL.

    lw_headdata-material = im_file_data-matnr.
    lw_plantdata-plant = im_file_data-werks.
    lw_plantdatax-plant = im_file_data-werks.

    " Save the material memo information
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        headdata         = lw_headdata
        plantdata        = lw_plantdata
        plantdatax       = lw_plantdatax
      IMPORTING
        return           = lw_return
      TABLES
        materiallongtext = im_material_memo
      EXCEPTIONS
        error_message    = 1
        OTHERS           = 2.

    CHECK sy-subrc IS INITIAL
    AND lw_return IS NOT INITIAL.

    me->add_log_data( " Set operation log
        im_file_line = im_file_line
        im_file_data = im_file_data
        im_bapiret   = VALUE bapiret2_t( ( lw_return ) )
    ).

    " Stop line processing and go to the next update operation
    IF lw_return-type = lc_error.
      RAISE EXCEPTION TYPE lcx_update_error.
    ENDIF.

  ENDMETHOD.

  METHOD log_display.

    DATA lr_alv TYPE REF TO cl_salv_table.

    CONSTANTS lc_error TYPE c LENGTH 1 VALUE 'E'.

    TRY.
        cl_salv_table=>factory(
            IMPORTING
                r_salv_table = lr_alv    " Basis Class Simple ALV Tables
            CHANGING
                t_table      = me->log
        ).
      CATCH cx_salv_msg.
        MESSAGE text-004 TYPE lc_error.
        LEAVE LIST-PROCESSING.
    ENDTRY.

    CHECK lr_alv IS BOUND.

    " Set all ALV toolbar buttons as available
    lr_alv->get_functions( )->set_all( ).

    me->handle_report_columns(
        CHANGING
            ch_alv = lr_alv
    ).

    lr_alv->display( ).

  ENDMETHOD.

  METHOD add_log_data.

    CONSTANTS lc_success TYPE c LENGTH 1 VALUE 'S'.
    CONSTANTS lc_warning TYPE c LENGTH 1 VALUE 'W'.
    CONSTANTS lc_error TYPE c LENGTH 1 VALUE 'E'.
    CONSTANTS lc_info TYPE c LENGTH 1 VALUE 'I'.

    INSERT LINES OF VALUE lcl_main=>tt_log(
        FOR lw_bapiret IN im_bapiret (
            fline      = im_file_line
            ekorg      = im_file_data-ekorg
            ekgrp      = im_file_data-ekgrp
            lifnr      = im_file_data-lifnr
            werks      = im_file_data-werks
            status     = COND icon_d(
              WHEN lw_bapiret-type = lc_success
              THEN icon_green_light
              WHEN lw_bapiret-type = lc_warning
              THEN icon_yellow_light
              WHEN lw_bapiret-type = lc_info
              THEN icon_information
              WHEN lw_bapiret-type = lc_error
              THEN icon_red_light
            )
            type       = lw_bapiret-type
            id         = lw_bapiret-id
            number     = lw_bapiret-number
            message    = lw_bapiret-message
            log_no     = lw_bapiret-log_no
            log_msg_no = lw_bapiret-log_msg_no
            message_v1 = lw_bapiret-message_v1
            message_v2 = lw_bapiret-message_v2
            message_v3 = lw_bapiret-message_v3
            message_v4 = lw_bapiret-message_v4
            parameter  = lw_bapiret-parameter
            row        = lw_bapiret-row
            field      = lw_bapiret-field
            system     = lw_bapiret-system
        )
    ) INTO TABLE me->log.

  ENDMETHOD.

  METHOD import_file_data.

    CONSTANTS lc_info TYPE c LENGTH 1 VALUE 'I'.
    CONSTANTS lc_error TYPE c LENGTH 1 VALUE 'E'.

    " Read the excel file and translate it to an internal table
    TRY.
        r_result = zma0_cl_sfut_email_receiver=>factory(
          CONV string( im_filepath )
        )->get_file_data( ).
      CATCH zcx_excel. " Check if the file exists first
        MESSAGE text-005 TYPE lc_info DISPLAY LIKE lc_error.
        LEAVE LIST-PROCESSING.
    ENDTRY.

    " Check if the file has any data to be processed
    IF r_result IS INITIAL.
      MESSAGE text-005 TYPE lc_info DISPLAY LIKE lc_error.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDMETHOD.

  METHOD handle_report_columns.

    DATA lr_column TYPE REF TO cl_salv_column.
    DATA(lr_columns) = ch_alv->get_columns( ).

    " Hide columns from the BAPI return internal table structure
    LOOP AT me->get_hidden_columns( ) INTO DATA(lv_hidden_column).
      TRY.
          lr_column = lr_columns->get_column( lv_hidden_column ).
          IF lr_column IS BOUND.
            lr_column->set_visible( abap_false ).
            FREE lr_column.
          ENDIF.
        CATCH cx_salv_not_found.
          CONTINUE.
      ENDTRY.
    ENDLOOP.

    TRY. " Set message column presentation size
        lr_column = lr_columns->get_column( 'MESSAGE' ).
        IF lr_column IS BOUND.
          lr_column->set_output_length( 100 ).
          FREE lr_column.
        ENDIF.
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY. " Set file line text for presentation
        lr_column = lr_columns->get_column( 'FLINE' ).
        IF lr_column IS BOUND.
          lr_column->set_long_text( text-006 ).
          lr_column->set_medium_text( text-006 ).
          FREE lr_column.
        ENDIF.
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY. " Set status text for presentation
        lr_column = lr_columns->get_column( 'STATUS' ).
        IF lr_column IS BOUND.
          lr_column->set_long_text( text-007 ).
          lr_column->set_medium_text( text-007 ).
          lr_column->set_alignment( 3 ).
          FREE lr_column.
        ENDIF.
      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.

  METHOD get_hidden_columns.

    re_result = VALUE #(
      ( 'TYPE' ) ( 'ID' ) ( 'NUMBER' ) ( 'LOG_NO' )
      ( 'LOG_MSG_NO' ) ( 'MESSAGE_V1' ) ( 'MESSAGE_V2' )
      ( 'MESSAGE_V3' ) ( 'MESSAGE_V4') ( 'PARAMETER' )
      ( 'ROW' ) ( 'FIELD' ) ( 'SYSTEM' )
    ).

  ENDMETHOD.

  METHOD convert_date.

    CHECK im_date IS NOT INITIAL.

    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        date_internal            = im_date
      IMPORTING
        date_external            = re_result
      EXCEPTIONS
        date_internal_is_invalid = 1
        error_message            = 2
        OTHERS                   = 3.

  ENDMETHOD.

  METHOD add_system_log.

    " Add the system message as a BAPIRET structure
    " for further processing
    CALL FUNCTION 'CMAC_APPEND_MESSAGE_TABLE'
      EXPORTING
        iv_msgtype    = im_syst-msgty
        iv_msgclass   = im_syst-msgid
        iv_msgnumber  = im_syst-msgno
        iv_msgv1      = im_syst-msgv1
        iv_msgv2      = im_syst-msgv2
        iv_msgv3      = im_syst-msgv3
        iv_msgv4      = im_syst-msgv4
      CHANGING
        ct_return     = ch_bapiret
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.

  ENDMETHOD.

  METHOD commit.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait          = abap_true
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.

  ENDMETHOD.

  METHOD roll_back.

    " Ignore changes on LUW in case of any error
    " during the BAPI update processing
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* Selection Screen
**********************************************************************

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-001.
PARAMETERS p_path TYPE zexcel_export_dir.
SELECTION-SCREEN END OF BLOCK a.

**********************************************************************
* Report Events
**********************************************************************

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.

  PERFORM get_file_path CHANGING p_path.

INITIALIZATION.

  PERFORM check_authorization.

START-OF-SELECTION.

  lcl_main=>run( p_path ).

**********************************************************************
* Sub-Routines
**********************************************************************

FORM check_authorization.

  " Check if the user have access to the transaction
  AUTHORITY-CHECK OBJECT 'S_TCODE'
  ID 'TCD' FIELD sy-tcode.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE text-003 TYPE 'E'.
  ENDIF.

ENDFORM.

FORM get_file_path CHANGING pv_path TYPE zexcel_export_dir.

  CONSTANTS lc_file_ext TYPE string VALUE '*.xlsx'.

  DATA lt_file_table TYPE filetable.
  DATA lv_action TYPE i.
  DATA lv_subrc TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      default_extension       = lc_file_ext
      default_filename        = lc_file_ext
      file_filter             = |{ text-002 } ({ lc_file_ext })|
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_subrc
      user_action             = lv_action
  ).

  pv_path = COND #(
    WHEN lv_action = cl_gui_frontend_services=>action_ok
    AND lv_subrc = 1
    THEN lt_file_table[ 1 ]
  ).

ENDFORM.
