"! <strong>Beispiel zur Verwendung des HL7-Renderers</strong>
"! <br/>
"! Verwenden Sie die Transaktion ZMCIBUCH_HL7_RENDER, um dieses Beispiel
"! auszuführen.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 5.2.5
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_hl7_render_demo DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    "! Erzeugt eine Nachricht im HL7-Objektmodell, wandelt sie in einen Text
    "! und zeigt sie an.
    METHODS execute.

  PROTECTED SECTION.

  PRIVATE SECTION.
    "! Erzeugt eine Nachricht im HL7-Objektmodell.
    METHODS create_hl7_message
      RETURNING
        VALUE(r_result) TYPE REF TO if_ishmed_mci_hl7_message.

ENDCLASS.



CLASS ZCL_MCIBUCH_HL7_RENDER_DEMO IMPLEMENTATION.


  METHOD create_hl7_message.
    r_result = NEW cl_ishmed_mci_hl7_message( ).
    DATA(segments) = r_result->get_segment_collection( ).
    segments->add( NEW cl_ishmed_mci_hl7_seg_msh_26(
                       is_message_type  = VALUE #( message_code      = 'ZZZ'
                                                   trigger_event     = 'SFT'
                                                   message_structure = 'ZZZ_SFT' )
    ) ).
    segments->add( NEW cl_ishmed_mci_hl7_seg_sft_26( ) ).
  ENDMETHOD.


  METHOD execute.

    " HL7-Nachricht im Objektmodell beschaffen
    DATA(hl7_message) = create_hl7_message( ).

    " Protokoll bereitstellen
    DATA(errorhandler) = NEW cl_ishmed_errorhandling( ).
    DATA(logger) = NEW cl_ishmed_mci_log_errorhandler(
        ir_errorhandler = errorhandler
    ).

    TRY.
        " HL7-Renderer initialisieren
        DATA(hl7_renderer) = CAST if_ishmed_mci_hl7_renderer(
                             NEW cl_ishmed_mci_hl7_pipe_render( ) ).
        hl7_renderer->init(
            i_type    = if_ishmed_mci_comp_constant=>co_renderer
            ir_logger = logger
         ).

        " Nachricht zum Rendern übergeben
        DATA hl7_text_message TYPE string.
        hl7_renderer->render(
          EXPORTING
            ir_message = hl7_message
            ir_logger  = logger
          IMPORTING
            e_value    = hl7_text_message
        ).

        " Nachricht zur Demonstration anzeigen
        IF hl7_text_message IS NOT INITIAL.
          cl_demo_output=>new( )->display(
              data = hl7_text_message
          ).
        ENDIF.

        " Instanzen wieder abbauen
        hl7_message->if_ishmed_object~finalize( ).
        hl7_renderer->if_ishmed_object~finalize( ).
        logger->if_ishmed_object~finalize( ).

      CATCH cx_ishmed_object cx_ishmed_mci INTO DATA(exception).
        errorhandler->append_message_for_exception( exception ).
    ENDTRY.

    " Meldungen anzeigen
    errorhandler->display_messages( ).

  ENDMETHOD.
ENDCLASS.
