CLASS zmal_cl_sfut_email_sender DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING VALUE(im_raw_data) TYPE zmal_sfut_supplier_data_t.

    CLASS-METHODS factory
      IMPORTING VALUE(im_raw_data) TYPE zmal_sfut_supplier_data_t
      RETURNING VALUE(re_instance)
                  TYPE REF TO zmal_cl_sfut_email_sender.

    METHODS send.

  PRIVATE SECTION.

    TYPES tt_email TYPE TABLE OF REF TO lcl_sfut_email
      WITH DEFAULT KEY.

    DATA emails TYPE tt_email.

    DATA database TYPE REF TO lcl_sfut_email_database.

    DATA raw_data TYPE zmal_sfut_supplier_data_t.

    METHODS prepare_emails.

    METHODS get_emails_to_be_sent
      RETURNING VALUE(re_emails_to_be_sent)
                  TYPE lif_sfut_email=>tt_receiver.

ENDCLASS.

CLASS zmal_cl_sfut_email_sender IMPLEMENTATION.

  METHOD constructor.
    " Attachment data w/o the split logic
    me->raw_data = im_raw_data.
    " Handle database access
    me->database = NEW #( im_email_data = me->raw_data ).
    " Split the raw data into the e-mails
    me->prepare_emails( ).
  ENDMETHOD.

  METHOD factory.
    re_instance = NEW #( im_raw_data ).
  ENDMETHOD.

  METHOD prepare_emails.

    " Iterate through each e-mail to be send based on the data at the
    " ZMAL_SFUT_RECV table (Receivers information)
    LOOP AT me->get_emails_to_be_sent( ) INTO DATA(lw_email_to_be_sent).

      " Create an instance with e-mail metadata information
      DATA(lr_details) = NEW lcl_sfut_email_details(
            im_database = me->database " Inject database information
            im_ekorg    = lw_email_to_be_sent-ekorg
            im_lifnr    = lw_email_to_be_sent-lifnr
            im_werks    = lw_email_to_be_sent-werks
      ).

      " Split the supplier attachment using the raw_data attribute
      DATA(lt_atch) = lr_details->split_attachment( me->raw_data ).

      " Avoid any e-mail if there is no attachment data
      CHECK lt_atch IS NOT INITIAL.

      " Set the attachment data
      lr_details->set_attachment( lt_atch ).

      " Append an instance of e-mail to the e-mails array attribute
      APPEND lcl_sfut_email=>factory( lr_details ) TO me->emails.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_emails_to_be_sent.

    " The instances of e-mail to be send will be created based on the
    " data registered at the receivers table (ZMAL_SFUT_RECV)
    re_emails_to_be_sent = me->database->get_receivers( ).
    DELETE ADJACENT DUPLICATES FROM re_emails_to_be_sent
    USING KEY primary_key.

  ENDMETHOD.

  METHOD send.

* Send each one of the prepared e-mails
    LOOP AT me->emails INTO DATA(lr_email).
      lr_email->send( ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
