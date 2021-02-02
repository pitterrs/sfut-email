CLASS zma0_cl_sfut_email_sender DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING VALUE(im_raw_data) TYPE zma0_sfut_supplier_data_t .

    CLASS-METHODS factory
      IMPORTING VALUE(im_raw_data) TYPE zma0_sfut_supplier_data_t
      RETURNING VALUE(re_instance)
                  TYPE REF TO zma0_cl_sfut_email_sender .

    METHODS send .

  PRIVATE SECTION.

    TYPES: tt_email TYPE TABLE OF REF TO lcl_sfut_email
           WITH DEFAULT KEY .

    DATA emails TYPE tt_email .
    DATA database TYPE REF TO lcl_sfut_email_database .
    DATA raw_data TYPE zma0_sfut_supplier_data_t .

    METHODS prepare_emails .

    METHODS get_emails_to_be_sent
      RETURNING VALUE(re_emails_to_be_sent)
                  TYPE lif_sfut_email=>tt_email_to_be_sent .
ENDCLASS.

CLASS zma0_cl_sfut_email_sender IMPLEMENTATION.

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

  METHOD get_emails_to_be_sent.

    " The instances of e-mail to be send will be created based on the
    " data registered at the receivers table (ZMA0_SFUT_RECV)

    DATA lw_email_to_be_sent TYPE LINE OF
        lif_sfut_email=>tt_email_to_be_sent.

    DATA lv_different_contacts TYPE abap_bool.

    DATA(lt_receivers) = me->database->get_receivers( ).
    DATA(lt_emails) = lt_receivers.

    DELETE ADJACENT DUPLICATES FROM lt_emails
    COMPARING ekorg lifnr werks.

    " Group the e-mails to be sent based on the contacts list of each
    " registered plant per Purchasing Organization/Supplier
    LOOP AT lt_emails INTO DATA(lw_email).

      lw_email_to_be_sent-ekorg = lw_email-ekorg.
      lw_email_to_be_sent-lifnr = lw_email-lifnr.
      APPEND VALUE #(
        sign = 'I'
        option = 'EQ'
        low = lw_email-werks
      ) TO lw_email_to_be_sent-plants.

      " Get contacts from this plant
      DATA(lt_this_contacts) = lt_receivers.
      DELETE lt_this_contacts
      WHERE ekorg <> lw_email-ekorg
      OR lifnr <> lw_email-lifnr
      OR werks <> lw_email-werks.

      " Get other plants with the same Purch Org and Supplier
      DATA(lt_other_plants) = lt_emails.
      DELETE lt_other_plants
      WHERE ( ekorg <> lw_email-ekorg
      OR      lifnr <> lw_email-lifnr )
      OR ( werks = lw_email-werks ).

      DELETE ADJACENT DUPLICATES FROM lt_other_plants
      COMPARING ekorg lifnr werks.

      LOOP AT lt_other_plants INTO DATA(lw_other_plant).

        " Get contacts from the other plant with the same PurchOrg/Sup
        DATA(lt_other_contacts) = lt_receivers.
        DELETE lt_other_contacts
        WHERE ( ekorg <> lw_other_plant-ekorg
        OR      lifnr <> lw_other_plant-lifnr
        OR      werks <> lw_other_plant-werks ).

        " Check if all the contacts of the current plant being processed
        " are the same on the other plant with the same PurchOrg/Sup
        lv_different_contacts = COND abap_bool(
            WHEN lines( lt_this_contacts ) = lines( lt_other_contacts )
            THEN abap_false ELSE abap_true
        ).
        IF lv_different_contacts IS INITIAL.
          LOOP AT lt_this_contacts INTO DATA(lw_this_contact).
            lv_different_contacts = COND abap_bool(
                WHEN line_exists(
                    lt_other_contacts[ email = lw_this_contact-email ]
                ) THEN abap_false ELSE abap_true
            ).
            IF lv_different_contacts IS NOT INITIAL.
              EXIT. " Don't need to continue the process... get out!
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF lv_different_contacts IS INITIAL.

          APPEND VALUE #(
            sign = 'I'
            option = 'EQ'
            low = lw_other_plant-werks
          ) TO lw_email_to_be_sent-plants.

          " Remove from the plant to be iterated because the same was
          " already added to one e-mail to be sent
          DELETE lt_emails
          WHERE ekorg = lw_other_plant-ekorg
          AND lifnr = lw_other_plant-lifnr
          AND werks = lw_other_plant-werks.

        ENDIF.

      ENDLOOP.

      APPEND lw_email_to_be_sent TO re_emails_to_be_sent.
      CLEAR lw_email_to_be_sent.
      FREE lw_email_to_be_sent-plants.

      " Remove from the plant to be iterated because the same was
      " already added to one e-mail to be sent
      DELETE lt_emails
      WHERE ekorg = lw_email-ekorg
      AND lifnr = lw_email-lifnr
      AND werks = lw_email-werks.

    ENDLOOP.

  ENDMETHOD.

  METHOD prepare_emails.

    " Iterate through each e-mail to be send based on the data at the
    " ZMA0_SFUT_RECV table (Receivers information)
    LOOP AT me->get_emails_to_be_sent( ) INTO DATA(lw_email_to_be_sent).

      " Create an instance with e-mail metadata information
      DATA(lr_details) = NEW lcl_sfut_email_details(
            im_database = me->database " Inject database information
            im_ekorg    = lw_email_to_be_sent-ekorg
            im_lifnr    = lw_email_to_be_sent-lifnr
            im_plants   = lw_email_to_be_sent-plants
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

  METHOD send.

    DATA lv_sent_any_message TYPE abap_bool.
    DATA lv_sent TYPE abap_bool.

* Send each one of the prepared e-mails
    LOOP AT me->emails INTO DATA(lr_email).
      TRY.
          lv_sent = lr_email->send( ).
        CATCH cx_document_bcs cx_send_req_bcs cx_address_bcs.
          CONTINUE. " Ignore post try/catch operations
      ENDTRY.
      IF lv_sent IS NOT INITIAL.
        lv_sent_any_message = abap_true.
        CLEAR lv_sent.
      ENDIF.
    ENDLOOP.

    IF lv_sent_any_message IS INITIAL.
      MESSAGE s002(zma0_sfut_email). " No e-mail has been sent
    ELSE.
      COMMIT WORK AND WAIT.
      MESSAGE s001(zma0_sfut_email). " The e-mail(s) has been sent
    ENDIF.

  ENDMETHOD.
ENDCLASS.
