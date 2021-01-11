*----------------------------------------------------------------------*
***INCLUDE LZMALV_SFUT_EMAILF01.
*----------------------------------------------------------------------*

FORM set_email_body .

  DATA lt_sfut_email LIKE TABLE OF zmalv_sfut_email_extract.

  DATA lv_index TYPE i.

  FIELD-SYMBOLS <xfrom> TYPE x. " Hexadecimal value of from value
  FIELD-SYMBOLS <xto> TYPE x. " Hexadecimal value of to value

  GET CURSOR LINE lv_index. " Get the selected row

* Load data in a auxiliary internal table for the read operation
* This logic is necessary due to the way the cluster views handles
* the data within the "extract" internal table
  LOOP AT extract.
    APPEND INITIAL LINE TO lt_sfut_email ASSIGNING <xto> CASTING.
    ASSIGN extract TO <xfrom> CASTING.
    <xto> = <xfrom>.
  ENDLOOP.

  DATA(lw_selected_row) = lt_sfut_email[ lv_index ].

  CHECK sy-subrc IS INITIAL
  AND lw_selected_row-ekorg IS NOT INITIAL.

  DATA(lv_display_mode) = COND #(
    WHEN maint_mode = co_display_mode
    THEN abap_true ELSE abap_false
  ).

  zmal_cl_sfut_email_helper=>factory(
    lw_selected_row-ekorg
  )->open_text_editor(
    im_display = lv_display_mode
  ).

ENDFORM.

FORM handle_email_button.

  FIELD-SYMBOLS <xfrom> TYPE x. " Hexadecimal value of from value
  FIELD-SYMBOLS <xto> TYPE x. " Hexadecimal value of to value

  DATA lw_current_line LIKE LINE OF zmalv_sfut_email_extract.

* Load the current line being processed at the table control
* loop to the variable lw_current_line to be handled internally
  ASSIGN extract TO <xfrom> CASTING.
  ASSIGN lw_current_line TO <xto> CASTING.
  <xto> = <xfrom>.

  LOOP AT SCREEN.
    IF screen-group1 = co_email_button. " BTN
* Check if the register has already been saved, in this case, the
* action column is cleared. So only after it is confirmed that is
* saved, we must enable the email body button
      IF lw_current_line-action IS INITIAL.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM delete_text_component.

  CONSTANTS lc_delete_operation TYPE c LENGTH 1 VALUE 'D'.

  DATA lt_sfut_email LIKE TABLE OF zmalv_sfut_email_total.

  FIELD-SYMBOLS <xfrom> TYPE x. " Hexadecimal value of from value
  FIELD-SYMBOLS <xto> TYPE x. " Hexadecimal value of to value

* Load data in a auxiliary internal table for the read operation
* This logic is necessary due to the way the cluster views handles
* the data within the "total" internal table
  LOOP AT total.
    APPEND INITIAL LINE TO lt_sfut_email ASSIGNING <xto> CASTING.
    ASSIGN total TO <xfrom> CASTING.
    <xto> = <xfrom>.
  ENDLOOP.

  DELETE lt_sfut_email
  WHERE action <> lc_delete_operation.

  LOOP AT lt_sfut_email INTO DATA(lw_sfut_email).
    zmal_cl_sfut_email_helper=>factory(
      lw_sfut_email-ekorg
    )->delete_text( ).
  ENDLOOP.

* Clear the result system variable to allow the standard to
* continue with the save operation
  CLEAR: sy-subrc.

ENDFORM.
