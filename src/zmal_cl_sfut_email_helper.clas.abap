CLASS zmal_cl_sfut_email_helper DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES tt_tline TYPE STANDARD TABLE OF tline
           WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING VALUE(im_langu)       TYPE syst-langu
                VALUE(im_purch_group) TYPE ekko-ekorg.

    CLASS-METHODS factory
      IMPORTING VALUE(im_langu)       TYPE syst-langu OPTIONAL
                VALUE(im_purch_group) TYPE ekko-ekorg
      RETURNING VALUE(re_helper)
                  TYPE REF TO zmal_cl_sfut_email_helper.

    METHODS read_text " Read operation
      RETURNING VALUE(re_lines) TYPE tt_tline.

    METHODS delete_text. " Delete operation

    METHODS open_text_editor " Create/Update through the SO10 screen
      IMPORTING VALUE(im_display) TYPE abap_bool OPTIONAL.

    METHODS get_object_name
      RETURNING VALUE(re_object_name) TYPE thead-tdname.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA purch_group TYPE ekko-ekorg.

    DATA langu TYPE syst-langu.

    CONSTANTS text_id TYPE thead-tdid VALUE `ST`. " Standard Text

    CONSTANTS object_type TYPE thead-tdobject VALUE `TEXT`.

    CONSTANTS max_line_size TYPE thead-tdlinesize VALUE 72.

ENDCLASS.

CLASS zmal_cl_sfut_email_helper IMPLEMENTATION.

  METHOD constructor.

* The subject and body of the e-mail will be defined for each specific
* purchasing group
    me->purch_group = im_purch_group.
    me->langu = im_langu.

  ENDMETHOD.

  METHOD factory.

* The language must be defined in order to properly maintain the
* so10 transaction text component
    DATA(lv_langu) = COND #(
        WHEN im_langu IS INITIAL THEN sy-langu ELSE im_langu
    ).

    re_helper = NEW #(
        im_langu       = lv_langu
        im_purch_group = im_purch_group
    ).

  ENDMETHOD.

  METHOD read_text.

* Retrieve the e-mail message body stored through the component created
* for that specific purchase group
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = zmal_cl_sfut_email_helper=>text_id
        language                = me->langu
        name                    = me->get_object_name( )
        object                  = zmal_cl_sfut_email_helper=>object_type
      TABLES
        lines                   = re_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        error_message           = 8
        OTHERS                  = 9.

  ENDMETHOD.

  METHOD delete_text.

* Delete all text components related to that purchasing group e-mail
    CALL FUNCTION 'DELETE_TEXT'
      EXPORTING
        id            = zmal_cl_sfut_email_helper=>text_id
        language      = '*' " Delete for all languages
        name          = me->get_object_name( )
        object        = zmal_cl_sfut_email_helper=>object_type
      EXCEPTIONS
        not_found     = 1
        error_message = 2
        OTHERS        = 3.

  ENDMETHOD.

  METHOD open_text_editor.

    DATA lw_header TYPE thead.

    lw_header-tdobject   = zmal_cl_sfut_email_helper=>object_type.
    lw_header-tdname     = me->get_object_name( ).
    lw_header-tdid       = zmal_cl_sfut_email_helper=>text_id.
    lw_header-tdspras    = me->langu.
    lw_header-tdlinesize = zmal_cl_sfut_email_helper=>max_line_size.

    DATA(lt_lines) = me->read_text( ). " Get e-mail body

* Calls the modification screen at the so10 transaction to enable the
* user to change the e-mail body that will be send to the supplier
    CALL FUNCTION 'EDIT_TEXT'
      EXPORTING
        display       = im_display
        header        = lw_header
      TABLES
        lines         = lt_lines
      EXCEPTIONS
        id            = 1
        language      = 2
        linesize      = 3
        name          = 4
        object        = 5
        textformat    = 6
        communication = 7
        error_message = 8
        OTHERS        = 9.

  ENDMETHOD.

  METHOD get_object_name.

* Get text component that stores the e-mail body
* Be aware that any change on the text component name composition will
* lose track of the already created objects on the application server
    re_object_name = |ZMAL_SFUT_EMAIL_{ me->purch_group }|.

  ENDMETHOD.

ENDCLASS.
