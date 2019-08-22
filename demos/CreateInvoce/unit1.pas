unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ETradeDoc, InvoceExchangeFile, Signer, InvoiceItem, zvlrpok_unit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ETradeDoc1: TETradeDoc;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FZvlrpokFrame: TZvlrpokFrame;
    procedure LoadConfig;
    procedure SaveConfig;
  public

  end;

var
  Form1: TForm1;

procedure InitLocale;
implementation
uses xmliconv, IniFiles;

{$R *.lfm}

function ConfigFileIni:TIniFile;
var
  S: String;
begin
  S:=GetAppConfigFile(false, true);
  Result:=TIniFile.Create(S);
end;

procedure ConvetToUTF8LocalConst;
var
  i:integer;
begin
  DefaultFormatSettings.ShortMonthNames[1]:='Январь';
  DefaultFormatSettings.ShortMonthNames[2]:='Февраль';
  DefaultFormatSettings.ShortMonthNames[3]:='Март';
  DefaultFormatSettings.ShortMonthNames[4]:='Апрель';
  DefaultFormatSettings.ShortMonthNames[5]:='Май';
  DefaultFormatSettings.ShortMonthNames[6]:='Июнь';
  DefaultFormatSettings.ShortMonthNames[7]:='Июль';
  DefaultFormatSettings.ShortMonthNames[8]:='Август';
  DefaultFormatSettings.ShortMonthNames[9]:='Сентябрь';
  DefaultFormatSettings.ShortMonthNames[10]:='Октябрь';
  DefaultFormatSettings.ShortMonthNames[11]:='Ноябрь';
  DefaultFormatSettings.ShortMonthNames[12]:='Декабрь';

  for i:=1 to 12 do
    DefaultFormatSettings.LongMonthNames[i] := DefaultFormatSettings.ShortMonthNames[i];

  DefaultFormatSettings.LongDayNames[1]:='Воскресенье';
  DefaultFormatSettings.LongDayNames[2]:='Понедельник';
  DefaultFormatSettings.LongDayNames[3]:='Вторник';
  DefaultFormatSettings.LongDayNames[4]:='Среда';
  DefaultFormatSettings.LongDayNames[5]:='Четверг';
  DefaultFormatSettings.LongDayNames[6]:='Пятница';
  DefaultFormatSettings.LongDayNames[7]:='Суббота';

  DefaultFormatSettings.ShortDayNames[1]:='Вс';
  DefaultFormatSettings.ShortDayNames[2]:='Пн';
  DefaultFormatSettings.ShortDayNames[3]:='Вт';
  DefaultFormatSettings.ShortDayNames[4]:='Ср';
  DefaultFormatSettings.ShortDayNames[5]:='Чт';
  DefaultFormatSettings.ShortDayNames[6]:='Пт';
  DefaultFormatSettings.ShortDayNames[7]:='Сб';
end;

procedure InitLocale;
begin
  //
  DefaultFormatSettings.LongDateFormat:='dd.mm.yyyy';
  DefaultFormatSettings.ShortDateFormat:=DefaultFormatSettings.LongDateFormat;
{.$IFNDEF MSWindows}
  DefaultFormatSettings.DateSeparator:='.';
  DefaultFormatSettings.TimeSeparator:=':';
{.$ELSE}
  ConvetToUTF8LocalConst;
{.$endif}
  DefaultFormatSettings.ThousandSeparator:=' ';
  DefaultFormatSettings.CurrencyString:='р.';
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  E: TExchangeFile;
  Ch: TSigner;
  II: TInvoiceItem;
  CD: TCustomsDeclaration;
  PIN: TProductIdentificationNumber;
  TI: TTraceabilityInformation;
begin
  E:=TExchangeFile.Create;
  E.FileID:='AASSDSADADAD-ADDAD-ADAD';
  E.FormatVersion:='5.1';
  E.AppVersion:='TestAPP XML 1.0';
  E.ParticipantsInformation.SenderInfo:='GUID-Отправитель';
  E.ParticipantsInformation.RecipientInfo:='GUID-Получатель';
  E.ParticipantsInformation.SellerExchangeInformation.FullName:='Организация отправитель';
  E.ParticipantsInformation.SellerExchangeInformation.Inn:='100000009';
  E.ParticipantsInformation.SellerExchangeInformation.IdentifierSenderOperator:='2DD';

  E.Document.KND:='1115125';
  E.Document.DocFunction:='СЧФДОП';
  E.Document.DocumentNameEL:='Документ об отгрузке товара';
  E.Document.DocumentName:='Документ об отгрузке товара';
  E.Document.InvoceDateCreate:='01.01.2019';
  E.Document.InvoceTimeCreate:='15.00.25';
  E.Document.DocumentCreator:='Организация';
(*  E.Document.DocumentCreatorBase:string read FDocumentCreatorBase write SetDocumentCreatorBase;
  E.Document.InformationAvailabilityStructureAdditionalInfo:string read FInformationAvailabilityStructureAdditionalInfo write SetInformationAvailabilityStructureAdditionalInfo;
  E.Document.InvoiceInformation:TInvoiceInformation read FInvoiceInformation; *)

  II:=E.Document.InvoiceItems.InvoiceItemList.CreateChild;
  II.LineNumber:=1;
  II.Product:='Товар 1';
  II.UnitCode:='615';
  //II.UnitCodeDef:string read FUnitCodeDef write SetUnitCodeDef;
  II.Quantity:='12.0';
  II.Price:='123';
  II.SubtotalWithVatExcluded:='12';
  II.TaxRate:='20%';
  II.Subtotal:='1234.00';
  //II.SubtotalDef:string read FSubtotalDef write SetSubtotalDef;
  //II.Excise:TExciseSum read FExcise;

  II.Vat.VatValue:='20.00';
  //II.Vat.WithoutVat:string read FWithoutVat write SetWithoutVat;
  //II.Vat.VatValueDef:string read FVatValueDef write SetVatValueDef;

  CD:=II.CustomsDeclaration.CreateChild;
  CD.CountryCode:='810';
  //CD.CountryCodeDef:string read FCountryCodeDef write SetCountryCodeDef;
  CD.DeclarationNumber:='123-123-123';
  II.InvoiceItemAdditional.ItemMark:='1';
  II.InvoiceItemAdditional.AdditionalProperty:='#3R';
  II.InvoiceItemAdditional.UnitName:='ШТ';
  II.InvoiceItemAdditional.CountryNameShort:='Россия';
  II.InvoiceItemAdditional.ItemToRelease:='123';
  II.InvoiceItemAdditional.ItemCharacteristic:='Характеристика';
  II.InvoiceItemAdditional.ItemSort:='1';
  II.InvoiceItemAdditional.ItemArticleNumber:='111111';
  II.InvoiceItemAdditional.ItemVendorCode:='2222';
  II.InvoiceItemAdditional.ItemCatalogCode:='333333';
  II.InvoiceItemAdditional.ItemTypeCode:='22 33 2342';
  TI:=II.InvoiceItemAdditional.TraceabilityInformation.CreateChild;
  TI.BatchNumber:='12-312-123';
  TI.UnitCode:='654';
  TI.UnitName:='Шт.';
  TI.Quantity:='123';
  TI.AdditionalInfo:='Примечание';

  PIN:=II.InvoiceItemAdditional.ProductIdentificationNumber.CreateChild;
  PIN.PackagingIdentificationNumber:='12312312313';
  //PIN.CheckMark:TStrings read FCheckMark;
  //PIN.SecondaryPackagingIdentificationNumber:TStrings read FSecondaryPackagingIdentificationNumber;

  //II.AdditionalInfo:TTextInfoList read FAdditionalInfo;


//  E.Document.TransferInfo:TTransferInfo read FTransferInfo;
  Ch:=E.Document.Signers.CreateChild;
  CH.SignerPowers:='1';
  CH.SignerStatus:='1';
  CH.SignerPowersBase:='Доверенность';
  CH.SignerOrgPowersBase:='Документ';
  CH.LegalEntityEmployee.RegistrationInfo:='Организация';
  CH.LegalEntityEmployee.Inn:='123456789';
  CH.LegalEntityEmployee.OrganizationName:='ООО Чебурашка';
  CH.LegalEntityEmployee.Position:='Сотрудник';
  CH.LegalEntityEmployee.OtherInfo:='Нет';
  CH.LegalEntityEmployee.Person.Surname:='Иванов';
  CH.LegalEntityEmployee.Person.FirstName:='Иван';
  CH.LegalEntityEmployee.Person.Patronymic:='Иванович';


  Ch:=E.Document.Signers.CreateChild;
  CH.SignerPowers:='2';
  CH.SignerStatus:='1';
  CH.SignerPowersBase:='Доверенность';
  CH.SignerOrgPowersBase:='Документ';
  CH.PhysicalPersonEntity.CertificateStateRegistration:='Свидетельство о гос регистрации';
  CH.PhysicalPersonEntity.INN:='123456789123';
  CH.PhysicalPersonEntity.OtherInfo:='Примечание';
  CH.PhysicalPersonEntity.Person.Surname:='Иванов';
  CH.PhysicalPersonEntity.Person.FirstName:='Иван';
  CH.PhysicalPersonEntity.Person.Patronymic:='Иванович';


  Ch:=E.Document.Signers.CreateChild;
  CH.SignerPowers:='3';
  CH.SignerStatus:='1';
  CH.SignerPowersBase:='Доверенность';
  CH.SignerOrgPowersBase:='Документ';
  CH.IndividualEntrepreneurInformation.INN:='123456789';
  //CH.IndividualEntrepreneurInformation.INNDef:string read FINNDef write SetINNDef;
  CH.IndividualEntrepreneurInformation.IndividualEntityRegistrationCertificate:='Сертификаты';
  CH.IndividualEntrepreneurInformation.OtherInfo:='Примечание';
  CH.IndividualEntrepreneurInformation.Person.Surname:='Иванов';
  CH.IndividualEntrepreneurInformation.Person.FirstName:='Иван';
  CH.IndividualEntrepreneurInformation.Person.Patronymic:='Иванович';


  ETradeDoc1.SaveInvoce(E, 'test1_invoce.xml');
  E.Free;
  ShowMessage('Успешно');
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FZvlrpokFrame:=TZvlrpokFrame.Create(Self);
  FZvlrpokFrame.Parent:=TabSheet3;
  FZvlrpokFrame.Align:=alClient;

  LoadConfig;
end;

procedure TForm1.LoadConfig;
var
  Ini: TIniFile;
begin
  Ini:=ConfigFileIni;
  FZvlrpokFrame.LoadData(Ini);
  Ini.Free;
end;

procedure TForm1.SaveConfig;
var
  Ini: TIniFile;
begin
  Ini:=ConfigFileIni;
  FZvlrpokFrame.SaveData(Ini);
  Ini.Free;
end;

end.

