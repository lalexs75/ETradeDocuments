unit zvlrpok_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, EditBtn, ComCtrls, IniFiles, DB,
  ImportGoodsAndIndirectTaxesExchangeFile, ImportGoodsAndIndirectTaxesDocument,
  EImportAndPayTaxDoc, rxmemds, rxdbgrid, DividerBevel;

type

  { TZvlrpokFrame }

  TZvlrpokFrame = class(TFrame)
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    DividerBevel1: TDividerBevel;
    dsItems: TDataSource;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    Edit17: TEdit;
    Edit18: TEdit;
    Edit19: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    EImportAndPayTaxDoc1: TEImportAndPayTaxDoc;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    RxDBGrid1: TRxDBGrid;
    rxItems: TRxMemoryData;
    rxItemsCurrencyBase: TStringField;
    rxItemsCurrencyCode: TStringField;
    rxItemsCurrencyRate: TStringField;
    rxItemsExcise: TStringField;
    rxItemsExciseBase: TStringField;
    rxItemsExciseFlag: TStringField;
    rxItemsExciseUnitCode: TStringField;
    rxItemsInvoiceDate: TDateField;
    rxItemsInvoiceNumber: TStringField;
    rxItemsLineNo: TLongintField;
    rxItemsPrice: TStringField;
    rxItemsProductDetailDocs: TStringField;
    rxItemsProductName: TStringField;
    rxItemsQuantity: TStringField;
    rxItemsRegistrationDate: TDateField;
    rxItemsTaxBase: TStringField;
    rxItemsTaxBase1: TStringField;
    rxItemsTaxBase2: TStringField;
    rxItemsTNVED: TStringField;
    rxItemsUnitCode: TStringField;
    rxItemsVat: TStringField;
    rxItemsVatFlag: TStringField;
    rxItemsVatRate: TStringField;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure Button1Click(Sender: TObject);
  private
    procedure LoadT4_1(ADoc:TImportGoodsAndIndirectTaxesExchangeFile);
    procedure LoadT4_2(ADocument:TImportGoodsAndIndirectTaxesDocument);
    procedure LoadT4_3(ASenderInfo:TSenderInfo);
    procedure LoadT4_4(ASignerInfo:TSignerInfo);
    procedure LoadT4_5(ADeclarationInfo:TDeclarationInfo);
    procedure LoadT4_6(ASellerContractInfo:TSellerContractInfo);

    procedure LoadT4_8(AProductDetails:TProductDetails);
  public
    procedure LoadData(AIniFile:TIniFile);
    procedure SaveData(AIniFile:TIniFile);
  end;

implementation

{$R *.lfm}

{ TZvlrpokFrame }

procedure TZvlrpokFrame.Button1Click(Sender: TObject);
var
  FDoc: TImportGoodsAndIndirectTaxesExchangeFile;
begin
  FDoc:=TImportGoodsAndIndirectTaxesExchangeFile.Create;
  FDoc.LoadFromXML(FileNameEdit1.FileName);
  FDoc.SaveToXML('aaa.xml');

  LoadT4_1(FDoc);

  FDoc.Free;

  FDoc:=EImportAndPayTaxDoc1.LoadGoodsAndPayFile(FileNameEdit1.FileName);
  FDoc.Free;
end;

procedure TZvlrpokFrame.LoadT4_1(ADoc: TImportGoodsAndIndirectTaxesExchangeFile
  );
begin
  Edit1.Text:=ADoc.FileID;
  Edit2.Text:=ADoc.FormatVersion;
  Edit3.Text:=ADoc.InformationType;
  Edit4.Text:=ADoc.ApplicationVersion;
  Edit5.Text:=ADoc.RecipientTaxInspectionCode;

  LoadT4_2(ADoc.Document);
end;

procedure TZvlrpokFrame.LoadT4_2(ADocument: TImportGoodsAndIndirectTaxesDocument
  );
begin
  Edit6.Text:=ADocument.KND;
  Edit7.Text:=ADocument.DocumentDate;
  LoadT4_3(ADocument.SenderInfo);
  LoadT4_4(ADocument.SignerInfo);
  LoadT4_5(ADocument.DeclarationInfo);
end;

procedure TZvlrpokFrame.LoadT4_3(ASenderInfo: TSenderInfo);
begin
  Edit8.Text:=ASenderInfo.LegalEntityInformation.FullName + ' ' + ASenderInfo.LegalEntityInformation.INN + ' ' + ASenderInfo.LegalEntityInformation.KPP + ' || ' +
    ASenderInfo.PhysicalPerson.INN + ' ' + ASenderInfo.PhysicalPerson.Person.FirstName + ' ' + ASenderInfo.PhysicalPerson.Person.Patronymic + ' ' + ASenderInfo.PhysicalPerson.Person.Surname;
end;

procedure TZvlrpokFrame.LoadT4_4(ASignerInfo: TSignerInfo);
begin
  Edit9.Text:=ASignerInfo.SignerType + ' ' + ASignerInfo.INN + ' ' + ASignerInfo.Position + ' ' +
    ASignerInfo.Person.FirstName + ' ' + ASignerInfo.Person.Patronymic + ' ' + ASignerInfo.Person.Surname;
end;

procedure TZvlrpokFrame.LoadT4_5(ADeclarationInfo: TDeclarationInfo);
begin
  Edit10.Text:=ADeclarationInfo.DocumentNumber;
  Edit11.Text:=ADeclarationInfo.DocumentData;
  CheckBox1.Checked:=ADeclarationInfo.LeasingFlag='1';
  CheckBox1.Checked:=ADeclarationInfo.ContractRawMaterialsFlag='1';
  Edit12.Text:=ADeclarationInfo.ExciseBase;
  Edit13.Text:=ADeclarationInfo.Excise;
  Edit14.Text:=ADeclarationInfo.VatBase;
  Edit15.Text:=ADeclarationInfo.Vat;
  ComboBox1.Text:=ADeclarationInfo.DocumentBaseFlag;
  Edit16.Text:=ADeclarationInfo.DocumentTaxNumber;
  Edit17.Text:=ADeclarationInfo.DocumentTaxData;

  LoadT4_6(ADeclarationInfo.SellerContractInfo);
  LoadT4_8(ADeclarationInfo.ProductDetails);
end;

procedure TZvlrpokFrame.LoadT4_6(ASellerContractInfo: TSellerContractInfo);
begin
  Edit18.Text:=ASellerContractInfo.SellerBaikonurFlag + ' ' + ASellerContractInfo.SellerIdentificationCode + ' '+
    ASellerContractInfo.SellerOrgType + ' '+
    ASellerContractInfo.SellerName + ' '+
    ASellerContractInfo.SellerCountryCode + ' '+
    ASellerContractInfo.SellerAdress;

  Edit19.Text:=ASellerContractInfo.BuyerBaikonurFlag + ' '+
    ASellerContractInfo.BuyerINN + ' '+
    ASellerContractInfo.BuyerName + ' '+
    ASellerContractInfo.BuyerCountryCode + ' '+
    ASellerContractInfo.BuyerAdress;

  //property ContractInfo:TContractInfo read FContractInfo;//Сведения о контракте (договоре) Раздел 1 стр. 05

end;

procedure TZvlrpokFrame.LoadT4_8(AProductDetails: TProductDetails);
var
  Itm: TProductDetail;
  PDD: TTransferDoc;
  S: String;
begin
  rxItems.CloseOpen;
  for Itm in AProductDetails do
  begin
    rxItems.Append;
    rxItemsLineNo.AsInteger:=Itm.LineNo.ToInteger;
    rxItemsProductName.AsString:=Itm.ProductName;
    rxItemsTNVED.AsString:=Itm.TNVED;
    rxItemsUnitCode.AsString:=Itm.UnitCode;;
    rxItemsQuantity.AsString:=Itm.Quantity;
    rxItemsPrice.AsString:=Itm.Price;
    rxItemsCurrencyCode.AsString:=Itm.CurrencyCode;
    rxItemsCurrencyRate.AsString:=Itm.CurrencyRate;
    rxItemsCurrencyBase.AsString:=Itm.CurrencyBase;
    rxItemsInvoiceNumber.AsString:=Itm.InvoiceNumber;
    rxItemsInvoiceDate.AsDateTime:=StrToDate(Itm.InvoiceDate);
    rxItemsRegistrationDate.AsDateTime:=StrToDate(Itm.RegistrationDate);
    rxItemsExciseBase.AsString:=Itm.ExciseBase;
    rxItemsExciseUnitCode.AsString:=Itm.ExciseUnitCode;
    rxItemsTaxBase.AsString:=Itm.TaxBase;
    rxItemsTaxBase1.AsString:=Itm.TaxBase1;
    rxItemsTaxBase2.AsString:=Itm.TaxBase2;
    rxItemsVatRate.AsString:=Itm.VatRate;
    rxItemsExcise.AsString:=Itm.Excise;
    rxItemsVat.AsString:=Itm.Vat;
    rxItemsExciseFlag.AsString:=Itm.ExciseFlag;
    rxItemsVatFlag.AsString:=Itm.VatFlag;
    S:='';
    for PDD in Itm.ProductDetailDocs do
    begin
      if S<>'' then S:=S + ',';
      S:=S + PDD.TransferDocNumber + ' от ' + PDD.TransferDocDate;
    end;
    rxItemsProductDetailDocs.AsString:=S;


    rxItems.Post;
  end;
end;

procedure TZvlrpokFrame.LoadData(AIniFile: TIniFile);
begin
  FileNameEdit1.Text:=AIniFile.ReadString('ZvlrpokFrame', 'FileName1', '');
end;

procedure TZvlrpokFrame.SaveData(AIniFile: TIniFile);
begin
  AIniFile.WriteString('ZvlrpokFrame', 'FileName1', FileNameEdit1.Text);
end;

end.

