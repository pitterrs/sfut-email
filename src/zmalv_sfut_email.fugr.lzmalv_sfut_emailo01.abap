*----------------------------------------------------------------------*
***INCLUDE LZMALV_SFUT_EMAILO01.
*----------------------------------------------------------------------*

MODULE handle_current_line OUTPUT.

  PERFORM handle_email_button.

ENDMODULE.

MODULE handle_screen_fields OUTPUT.

* Always enable the e-mail body content button on display mode
  IF maint_mode = co_display_mode. " Display mode 'S'
    LOOP AT SCREEN.
      IF screen-group1 = co_email_button. " BTN
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.
