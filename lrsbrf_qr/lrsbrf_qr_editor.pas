unit lrSBRF_QR_Editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  EditBtn, LR_Class;

type

  { TlrSBRF_QR_EditorForm }

  TlrSBRF_QR_EditorForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    EditButton1: TEditButton;
    EditButton2: TEditButton;
    EditButton3: TEditButton;
    EditButton4: TEditButton;
    EditButton5: TEditButton;
    EditButton6: TEditButton;
    EditButton7: TEditButton;
    EditButton8: TEditButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure EditButton1ButtonClick(Sender: TObject);
  private

  public

  end;


function lrSBRF_QREditorProc(lrObj: TfrView) : boolean;
implementation
uses lrSBRF_QR, lr_expres;

function lrSBRF_QREditorProc(lrObj: TfrView): boolean;
var
  lrSBRF_QR_EditorForm: TlrSBRF_QR_EditorForm;
  L:TlrSBRF_QRCodeView;
begin
  Result:=false;
  if (not Assigned(lrObj)) or (not (lrObj is TlrSBRF_QRCodeView)) then Exit;
  L:=TlrSBRF_QRCodeView(lrObj);

  lrSBRF_QR_EditorForm:=TlrSBRF_QR_EditorForm.Create(Application);

  lrSBRF_QR_EditorForm.EditButton1.Text:=L.PayRecipientName;
  lrSBRF_QR_EditorForm.EditButton2.Text:=L.PayRecipientAccount;
  lrSBRF_QR_EditorForm.EditButton3.Text:=L.PayRecipientBank;
  lrSBRF_QR_EditorForm.EditButton4.Text:=L.PayRecipientBIC;
  lrSBRF_QR_EditorForm.EditButton5.Text:=L.PayRecipientCorrAccount;
  lrSBRF_QR_EditorForm.EditButton6.Text:=L.Sum;
  lrSBRF_QR_EditorForm.EditButton7.Text:=L.PayerINN;
  lrSBRF_QR_EditorForm.EditButton8.Text:=L.Purpose;

  if lrSBRF_QR_EditorForm.ShowModal = mrOk then
  begin
    L.PayRecipientName        := lrSBRF_QR_EditorForm.EditButton1.Text;
    L.PayRecipientAccount     := lrSBRF_QR_EditorForm.EditButton2.Text;
    L.PayRecipientBank        := lrSBRF_QR_EditorForm.EditButton3.Text;
    L.PayRecipientBIC         := lrSBRF_QR_EditorForm.EditButton4.Text;
    L.PayRecipientCorrAccount := lrSBRF_QR_EditorForm.EditButton5.Text;
    L.Sum                     := lrSBRF_QR_EditorForm.EditButton6.Text;
    L.PayerINN                := lrSBRF_QR_EditorForm.EditButton7.Text;
    L.Purpose                 := lrSBRF_QR_EditorForm.EditButton8.Text;
    //frDesigner.Modified:=true;
    Result:=true;
  end;
  lrSBRF_QR_EditorForm.Free;
end;

{$R *.lfm}

{ TlrSBRF_QR_EditorForm }

procedure TlrSBRF_QR_EditorForm.EditButton1ButtonClick(Sender: TObject);
var
  lrExpresionEditorForm: TlrExpresionEditorForm;
begin
  lrExpresionEditorForm:=TlrExpresionEditorForm.Create(Application);
  try
    lrExpresionEditorForm.Memo1.Text:=(Sender as TEditButton).Text;
    if lrExpresionEditorForm.ShowModal = mrOk then
      (Sender as TEditButton).Text:=lrExpresionEditorForm.ResultExpresion;
  finally
    lrExpresionEditorForm.Free;
  end;
end;

end.

