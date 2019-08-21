{ interface library for FPC and Lazarus

  Copyright (C) 2019 Lagunov Aleksey alexs75@yandex.ru

  Генерация xml файлов для электронного документооборота
  Формат заявления о ввозе товаров и уплате косвенных налогов российского налогоплательщика

  Структуры данных базируются на основании "Приказ от 19.11.2014 № ММВ-7-6/590@"

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit ImportGoodsAndIndirectTaxesDocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xml_doc;

type

  { TPerson }

  TPerson = class(TXmlSerializationObject)   //%Таблица 4.18
  private
    FFirstName: string;
    FPatronymic: string;
    FSurname: string;
    procedure SetFirstName(AValue: string);
    procedure SetPatronymic(AValue: string);
    procedure SetSurname(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Surname:string read FSurname write SetSurname;
    property FirstName:string read FFirstName write SetFirstName;
    property Patronymic:string read FPatronymic write SetPatronymic;
  end;

  { TPhysicalPersonEntity }

  TPhysicalPersonEntity = class(TXmlSerializationObject) //%Таблица 4.17
  private
    FINN: string;
    FPerson: TPerson;
    procedure SetINN(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property INN:string read FINN write SetINN;
    property Person:TPerson read FPerson;
  end;

  { TLegalEntityInformation }

  TLegalEntityInformation = class(TXmlSerializationObject) //%Таблица 4.16
  private
    FFullName: string;
    FINN: string;
    FKPP: string;
    procedure SetFullName(AValue: string);
    procedure SetINN(AValue: string);
    procedure SetKPP(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property FullName:string read FFullName write SetFullName;
    property INN:string read FINN write SetINN;
    property KPP:string read FKPP write SetKPP;
  end;

  { TAuthorizedInformation }

  TAuthorizedInformation = class(TXmlSerializationObject) //%Таблица 4.15
  private
    FDocumentDate: string;
    FDocumentName: string;
    FDocumentNumber: string;
    procedure SetDocumentDate(AValue: string);
    procedure SetDocumentName(AValue: string);
    procedure SetDocumentNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property DocumentName:string read FDocumentName write SetDocumentName; //Наименование документа (доверенности, приказа), подтверждающего полномочия представителя
    property DocumentNumber:string read FDocumentNumber write SetDocumentNumber; //Номер документа
    property DocumentDate:string read FDocumentDate write SetDocumentDate; //Дата документа
  end;

  { TSpecificationsInformation }

  TSpecificationsInformation = class(TXmlSerializationObject) //%Таблица 4.14
  private
    FOrderNumber: string;
    FSpecificationDate: string;
    FSpecificationNumber: string;
    procedure SetOrderNumber(AValue: string);
    procedure SetSpecificationDate(AValue: string);
    procedure SetSpecificationNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property OrderNumber:string read FOrderNumber write SetOrderNumber;//Номер по порядку
    property SpecificationNumber:string read FSpecificationNumber write SetSpecificationNumber;//Номер спецификации
    property SpecificationDate:string read FSpecificationDate write SetSpecificationDate;//Дата спецификации
  end;

  { TSpecificationsInformations }

  TSpecificationsInformations = class(TXmlSerializationObjectList) //%Таблица 4.14
  private
    function GetItem(AIndex: Integer): TSpecificationsInformation; inline;
  public
    constructor Create;
    function CreateChild:TSpecificationsInformation;
    property Item[AIndex:Integer]:TSpecificationsInformation read GetItem; default;
  end;

  { TContractInfo }

  TContractInfo = class(TXmlSerializationObject) //%Таблица 4.13
  private
    FContractDate: string;
    FContractNumber: string;
    FSpecificationsInformation: TSpecificationsInformations;
    procedure SetContractDate(AValue: string);
    procedure SetContractNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ContractNumber:string read FContractNumber write SetContractNumber;//Номер контракта
    property ContractDate:string read FContractDate write SetContractDate;//Дата контракта
    property SpecificationsInformation:TSpecificationsInformations read FSpecificationsInformation; //Сведения спецификаций
  end;

  { TTransferDoc }

  TTransferDoc = class(TXmlSerializationObject) //%Таблица 4.9
  private
    FTransferDocDate: string;
    FTransferDocNumber: string;
    procedure SetTransferDocDate(AValue: string);
    procedure SetTransferDocNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property TransferDocNumber:string read FTransferDocNumber write SetTransferDocNumber; //Серия, номер транспортного (товаросопроводительного) документа Графа 9
    property TransferDocDate:string read FTransferDocDate write SetTransferDocDate; //Дата транспортного (товаросопроводительного) документа Графа 10
  end;

  { TTransferDocs }

  TTransferDocs = class(TXmlSerializationObjectList) //%Таблица 4.9
  private
    function GetItem(AIndex: Integer): TTransferDoc; inline;
  public
    constructor Create;
    function CreateChild:TTransferDoc; inline;
    property Item[AIndex:Integer]:TTransferDoc read GetItem; default;
  end;

  { TProductDetail }

  TProductDetail = class(TXmlSerializationObject) //%Таблица 4.8
  private
    FCurrencyBase: string;
    FCurrencyCode: string;
    FCurrencyRate: string;
    FExcise: string;
    FExciseBase: string;
    FExciseFlag: string;
    FExciseUnitCode: string;
    FInvoiceDate: string;
    FInvoiceNumber: string;
    FLineNo: string;
    FPrice: string;
    FProductDetailDocs: TTransferDocs;
    FProductName: string;
    FQuantity: string;
    FRegistrationDate: string;
    FTaxBase: string;
    FTaxBase1: string;
    FTaxBase2: string;
    FTNVED: string;
    FUnitCode: string;
    FVat: string;
    FVatFlag: string;
    FVatRate: string;
    procedure SetCurrencyBase(AValue: string);
    procedure SetCurrencyCode(AValue: string);
    procedure SetCurrencyRate(AValue: string);
    procedure SetExcise(AValue: string);
    procedure SetExciseBase(AValue: string);
    procedure SetExciseFlag(AValue: string);
    procedure SetExciseUnitCode(AValue: string);
    procedure SetInvoiceDate(AValue: string);
    procedure SetInvoiceNumber(AValue: string);
    procedure SetLineNo(AValue: string);
    procedure SetPrice(AValue: string);
    procedure SetProductName(AValue: string);
    procedure SetQuantity(AValue: string);
    procedure SetRegistrationDate(AValue: string);
    procedure SetTaxBase(AValue: string);
    procedure SetTaxBase1(AValue: string);
    procedure SetTaxBase2(AValue: string);
    procedure SetTNVED(AValue: string);
    procedure SetUnitCode(AValue: string);
    procedure SetVat(AValue: string);
    procedure SetVatFlag(AValue: string);
    procedure SetVatRate(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property LineNo:string read FLineNo write SetLineNo;//Номер по порядку товара в документе Графа 1
    property ProductName:string read FProductName write SetProductName;//Наименование товара Графа 2
    property TNVED:string read FTNVED write SetTNVED;//Код товара ТНВЭД Графа 3
    property UnitCode:string read FUnitCode write SetUnitCode; //Единица измерения товара Графа 4
    property Quantity:string read FQuantity write SetQuantity; //Количество товара Графа 5
    property Price:string read FPrice write SetPrice; //Стоимость товара Графа 6
    property CurrencyCode:string read FCurrencyCode write SetCurrencyCode; //Код валюты Графа 7
    property CurrencyRate:string read FCurrencyRate write SetCurrencyRate; //Курс валюты Графа 8
    property CurrencyBase:string read FCurrencyBase write SetCurrencyBase; //База валюты
    property InvoiceNumber:string read FInvoiceNumber write SetInvoiceNumber; //Номер счета-фактуры Графа 11
    property InvoiceDate:string read FInvoiceDate write SetInvoiceDate; //Дата счета-фактуры Графа 12
    property RegistrationDate:string read FRegistrationDate write SetRegistrationDate; //Дата принятия на учет товара Графа 13
    property ExciseBase:string read FExciseBase write SetExciseBase; //Налоговая база (акциз) Графа 14
    property ExciseUnitCode:string read FExciseUnitCode write SetExciseUnitCode; //Единица измерения дополнительной величины, используемой для исчисления налоговой базы (акциз)
    property TaxBase:string read FTaxBase write SetTaxBase; //Налоговая база (НДС) Графа 15
    property TaxBase1:string read FTaxBase1 write SetTaxBase1; //Ставка налога акцизов твердых (специфических) Графа 16
    property TaxBase2:string read FTaxBase2 write SetTaxBase2; //Ставка налога акцизов адвалорных Графа 17
    property VatRate:string read FVatRate write SetVatRate; //Ставка налога (НДС) Графа 18
    property Excise:string read FExcise write SetExcise; //Сумма налога (акциз) Графа 19
    property Vat:string read FVat write SetVat; //Сумма налога (НДС) Графа 20
    property ExciseFlag:string read FExciseFlag write SetExciseFlag; //Признак освобождения от уплаты налога (акцизы)
    property VatFlag:string read FVatFlag write SetVatFlag; //Признак освобождения от уплаты налога (НДС)
    property ProductDetailDocs:TTransferDocs read FProductDetailDocs; //Сведения о товаросопроводительных документах
  end;

  { TProductDetails }

  TProductDetails = class(TXmlSerializationObjectList) //%Таблица 4.8
  private
    function GetItem(AIndex: Integer): TProductDetail; inline;
  public
    constructor Create;
    function CreateChild:TProductDetail; inline;
    property Item[AIndex:Integer]:TProductDetail read GetItem; default;
  end;

  { TCommissionContractInfo }

  TCommissionContractInfo = class(TXmlSerializationObject) //%Таблица 4.7
  private
    FAdress: string;
    FBaikonurFlag: string;
    FContractInfo: TContractInfo;
    FCountryCode: string;
    FIdentificationCode: string;
    FName: string;
    procedure SetAdress(AValue: string);
    procedure SetBaikonurFlag(AValue: string);
    procedure SetCountryCode(AValue: string);
    procedure SetIdentificationCode(AValue: string);
    procedure SetName(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property BaikonurFlag:string read FBaikonurFlag write SetBaikonurFlag; //Признак нахождения российского посредника в г.Байконур
    property IdentificationCode:string read FIdentificationCode write SetIdentificationCode; //Идентификационный код (номер) посредника Раздел 1 стр.06
    property Name:string read FName write SetName; //Наименование организации (ФИО индивидуального предпринимателя) посредника Раздел 1 стр.06
    property CountryCode:string read FCountryCode write SetCountryCode; //Код страны посредника Раздел 1 стр.06
    property Adress:string read FAdress write SetAdress; //Адрес местонахождения (жительства) посредника Раздел 1 стр.06
    property ContractInfo:TContractInfo read FContractInfo; //Сведения о контракте (договоре) Раздел 1 стр.07
  end;

  { TSellerContractInfo }

  TSellerContractInfo = class(TXmlSerializationObject) //%Таблица 4.6
  private
    FBuyerAdress: string;
    FBuyerBaikonurFlag: string;
    FBuyerCountryCode: string;
    FBuyerINN: string;
    FBuyerName: string;
    FContractInfo: TContractInfo;
    FSellerAdress: string;
    FSellerBaikonurFlag: string;
    FSellerCountryCode: string;
    FSellerIdentificationCode: string;
    FSellerName: string;
    FSellerOrgType: string;
    procedure SetBuyerAdress(AValue: string);
    procedure SetBuyerBaikonurFlag(AValue: string);
    procedure SetBuyerCountryCode(AValue: string);
    procedure SetBuyerINN(AValue: string);
    procedure SetBuyerName(AValue: string);
    procedure SetSellerAdress(AValue: string);
    procedure SetSellerBaikonurFlag(AValue: string);
    procedure SetSellerCountryCode(AValue: string);
    procedure SetSellerIdentificationCode(AValue: string);
    procedure SetSellerName(AValue: string);
    procedure SetSellerOrgType(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property SellerBaikonurFlag:string read FSellerBaikonurFlag write SetSellerBaikonurFlag; //Признак нахождения российского продавца в г.Байконур
    property SellerIdentificationCode:string read FSellerIdentificationCode write SetSellerIdentificationCode; //Идентификационный код (номер) продавца Раздел 1 стр. 01
    property SellerOrgType:string read FSellerOrgType write SetSellerOrgType;//Признак продавца – физического лица (не индивидуального предпринимателя)
    property SellerName:string read FSellerName write SetSellerName;//Полное наименование (ФИО) продавца Раздел 1 стр. 01
    property SellerCountryCode:string read FSellerCountryCode write SetSellerCountryCode;//Код страны продавца Раздел 1 стр. 03
    property SellerAdress:string read FSellerAdress write SetSellerAdress;//Адрес местонахождения (жительства) продавца Раздел 1 стр. 03
    property BuyerBaikonurFlag:string read FBuyerBaikonurFlag write SetBuyerBaikonurFlag; //Признак нахождения российского покупателя в г.Байконур
    property BuyerINN:string read FBuyerINN write SetBuyerINN; //ИНН покупателя Раздел 1 стр. 02
    property BuyerName:string read FBuyerName write SetBuyerName; //Полное наименование (ФИО) покупателя Раздел 1 стр. 02
    property BuyerCountryCode:string read FBuyerCountryCode write SetBuyerCountryCode;//Код страны покупателя Раздел 1 стр. 04
    property BuyerAdress:string read FBuyerAdress write SetBuyerAdress;//Адрес местонахождения (жительства) покупателя Раздел 1 стр. 04
    property ContractInfo:TContractInfo read FContractInfo;//Сведения о контракте (договоре) Раздел 1 стр. 05
  end;

  { TDeclarationInfo }

  TDeclarationInfo = class(TXmlSerializationObject) //%Таблица 4.5
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property DocumentNumber:string;//Номер заявления, указанный НП
    property DocumentData:string;//Дата заполнения заявления, указанная НП
    property LeasingFlag:string;//Признак договора лизинга
    property ContractRawMaterialsFlag:string;//Признак договора переработки давальческого сырья
    property ExciseBase:string; //База по акцизам Итого по графе 14 Раздел 1
    property VatBase:string; //База по НДС Итого по графе 15 Раздел 1
    property Excise:string; //Акциз в сумме Итого по графе 19 Раздел 1
    property Vat:string; //НДС в сумме Итого по графе 20 Раздел 1
    property DocumentBaseFlag:string;//Причина возникновения заявления
    property DocumentTaxNumber:string;//Номер отметки о регистрации в налоговом органе ранее представленного заявления
    property DocumentTaxData:string;//Дата отметки о регистрации в налоговом органе ранее представленного заявления
    property SellerContractInfo:TSellerContractInfo read FSellerContractInfo;//Сведения о договоре (контракте) Раздел 1 стр.05
    property CommissionContractInfo:TCommissionContractInfo read FCommissionContractInfo;//Сведения о контракте с комиссионером Раздел 1 стр. 06-07
    property ProductDetails:TProductDetails read FProductDetails;//Сведения о товаре и уплаченных налогах
    //Сведения о договоре (контракте) Раздел 3
    //Сведения о ранее представленном заявлении
  end;

  { TSignerInfo }

  TSignerInfo = class(TXmlSerializationObject) //%Таблица 4.4
  private
    FAuthorizedInformation: TAuthorizedInformation;
    FINN: string;
    FPerson: TPerson;
    FPosition: string;
    FSignerType: string;
    procedure SetINN(AValue: string);
    procedure SetPosition(AValue: string);
    procedure SetSignerType(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property SignerType:string read FSignerType write SetSignerType;  //Признак лица, подписавшего документ
    property Position:string read FPosition write SetPosition; //Должность лица, подписавшего документ
    property INN:string read FINN write SetINN; //ИНН физического лица
    property Person:TPerson read FPerson;  //Фамилия, имя, отчество
    property AuthorizedInformation:TAuthorizedInformation read FAuthorizedInformation; //Сведения об уполномоченном представителе
  end;

  { TSenderInfo }

  TSenderInfo = class(TXmlSerializationObject) //%Таблица 4.3
  private
    FLegalEntityInformation: TLegalEntityInformation;
    FPhysicalPerson: TPhysicalPersonEntity;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property LegalEntityInformation:TLegalEntityInformation read FLegalEntityInformation;
    property PhysicalPerson:TPhysicalPersonEntity read FPhysicalPerson;
  end;

  { TImportGoodsAndIndirectTaxesDocument }

  TImportGoodsAndIndirectTaxesDocument = class(TXmlSerializationObject) //%Таблица 4.2
  private
    FDeclarationInfo: TDeclarationInfo;
    FDocumentDate: string;
    FKND: string;
    FSenderInfo: TSenderInfo;
    FSignerInfo: TSignerInfo;
    procedure SetDocumentDate(AValue: string);
    procedure SetKND(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property KND:string read FKND write SetKND;  //Код формы по КНД
    property DocumentDate:string read FDocumentDate write SetDocumentDate; //Дата формирования документа
    property SenderInfo:TSenderInfo read FSenderInfo; //Сведения об отправителе документа
    property SignerInfo:TSignerInfo read FSignerInfo; //Сведения о лице, подписавшем документ
    property DeclarationInfo:TDeclarationInfo read FDeclarationInfo; //Сведения из заявления
//    property ContractAdditional:TContractAdditionals read FContracts; //Сведения о договорах (контрактах) приложения к Заявлению
//Отправитель – организация
//Отправитель - физическое лицо
  end;

implementation

{ TProductDetail }

procedure TProductDetail.SetCurrencyBase(AValue: string);
begin
  if FCurrencyBase=AValue then Exit;
  FCurrencyBase:=AValue;
  ModifiedProperty('CurrencyBase');
end;

procedure TProductDetail.SetCurrencyCode(AValue: string);
begin
  if FCurrencyCode=AValue then Exit;
  FCurrencyCode:=AValue;
  ModifiedProperty('CurrencyCode');
end;

procedure TProductDetail.SetCurrencyRate(AValue: string);
begin
  if FCurrencyRate=AValue then Exit;
  FCurrencyRate:=AValue;
  ModifiedProperty('CurrencyRate');
end;

procedure TProductDetail.SetExcise(AValue: string);
begin
  if FExcise=AValue then Exit;
  FExcise:=AValue;
  ModifiedProperty('Excise');
end;

procedure TProductDetail.SetExciseBase(AValue: string);
begin
  if FExciseBase=AValue then Exit;
  FExciseBase:=AValue;
  ModifiedProperty('ExciseBase');
end;

procedure TProductDetail.SetExciseFlag(AValue: string);
begin
  if FExciseFlag=AValue then Exit;
  FExciseFlag:=AValue;
  ModifiedProperty('ExciseFlag');
end;

procedure TProductDetail.SetExciseUnitCode(AValue: string);
begin
  if FExciseUnitCode=AValue then Exit;
  FExciseUnitCode:=AValue;
  ModifiedProperty('ExciseUnitCode');
end;

procedure TProductDetail.SetInvoiceDate(AValue: string);
begin
  if FInvoiceDate=AValue then Exit;
  FInvoiceDate:=AValue;
  ModifiedProperty('InvoiceDate');
end;

procedure TProductDetail.SetInvoiceNumber(AValue: string);
begin
  if FInvoiceNumber=AValue then Exit;
  FInvoiceNumber:=AValue;
  ModifiedProperty('InvoiceNumber');
end;

procedure TProductDetail.SetLineNo(AValue: string);
begin
  if FLineNo=AValue then Exit;
  FLineNo:=AValue;
  ModifiedProperty('LineNo');
end;

procedure TProductDetail.SetPrice(AValue: string);
begin
  if FPrice=AValue then Exit;
  FPrice:=AValue;
  ModifiedProperty('Price');
end;

procedure TProductDetail.SetProductName(AValue: string);
begin
  if FProductName=AValue then Exit;
  FProductName:=AValue;
  ModifiedProperty('ProductName');
end;

procedure TProductDetail.SetQuantity(AValue: string);
begin
  if FQuantity=AValue then Exit;
  FQuantity:=AValue;
  ModifiedProperty('Quantity');
end;

procedure TProductDetail.SetRegistrationDate(AValue: string);
begin
  if FRegistrationDate=AValue then Exit;
  FRegistrationDate:=AValue;
  ModifiedProperty('RegistrationDate');
end;

procedure TProductDetail.SetTaxBase(AValue: string);
begin
  if FTaxBase=AValue then Exit;
  FTaxBase:=AValue;
  ModifiedProperty('TaxBase');
end;

procedure TProductDetail.SetTaxBase1(AValue: string);
begin
  if FTaxBase1=AValue then Exit;
  FTaxBase1:=AValue;
  ModifiedProperty('TaxBase1');
end;

procedure TProductDetail.SetTaxBase2(AValue: string);
begin
  if FTaxBase2=AValue then Exit;
  FTaxBase2:=AValue;
  ModifiedProperty('TaxBase2');
end;

procedure TProductDetail.SetTNVED(AValue: string);
begin
  if FTNVED=AValue then Exit;
  FTNVED:=AValue;
  ModifiedProperty('TNVED');
end;

procedure TProductDetail.SetUnitCode(AValue: string);
begin
  if FUnitCode=AValue then Exit;
  FUnitCode:=AValue;
  ModifiedProperty('UnitCode');
end;

procedure TProductDetail.SetVat(AValue: string);
begin
  if FVat=AValue then Exit;
  FVat:=AValue;
  ModifiedProperty('Vat');
end;

procedure TProductDetail.SetVatFlag(AValue: string);
begin
  if FVatFlag=AValue then Exit;
  FVatFlag:=AValue;
  ModifiedProperty('VatFlag');
end;

procedure TProductDetail.SetVatRate(AValue: string);
begin
  if FVatRate=AValue then Exit;
  FVatRate:=AValue;
  ModifiedProperty('VatRate');
end;

procedure TProductDetail.InternalRegisterPropertys;
begin
  RegisterProperty('LineNo', 'НомТовПП', 'О', 'Номер по порядку товара в документе Графа 1', 0, 4);
  RegisterProperty('ProductName', 'НаимТов', 'О', 'Наименование товара Графа 2', 1, 500);
  RegisterProperty('TNVED', 'ТНВЭД', 'НК', 'Код товара ТНВЭД Графа 3', 10, 10);
  RegisterProperty('UnitCode', 'ЕдИзмТов', 'ОК', 'Единица измерения товара Графа 4', 3, 4);
  RegisterProperty('Quantity', 'КоличТов', 'О', 'Количество товара Графа 5', 1, 17);
  RegisterProperty('Price', 'СтоимТов', 'О', 'Стоимость товара Графа 6', 1, 16);
  RegisterProperty('CurrencyCode', 'ВалТов', 'ОК', 'Код валюты Графа 7', 3, 3);
  RegisterProperty('CurrencyRate', 'КурсВал', 'О', 'Курс валюты Графа 8', 1, 10);
  RegisterProperty('CurrencyBase', 'БазаВал', 'О', 'База валюты', 1, 5);
  RegisterProperty('InvoiceNumber', 'НомСчФ', 'О', 'Номер счета-фактуры Графа 11', 1, 50);
  RegisterProperty('InvoiceDate', 'ДатаСчФ', 'О', 'Дата счета-фактуры Графа 12', 10, 10);
  RegisterProperty('RegistrationDate', 'ДатаПрин', 'О', 'ДатаПрин', 10, 10);
  RegisterProperty('ExciseBase', 'НБАкциз', 'Н', 'Налоговая база (акциз) Графа 14', 1, 21);
  RegisterProperty('ExciseUnitCode', 'ЕдИзмТовНБАкц', 'НК', 'Единица измерения дополнительной величины, используемой для исчисления налоговой базы (акциз)', 3, 4);
  RegisterProperty('TaxBase', 'НБНДС', 'О', 'Налоговая база (НДС) Графа 15', 1, 16);
  RegisterProperty('TaxBase1', 'СтАкцизТверд', 'Н', 'Ставка налога акцизов твердых (специфических) Графа 16', 1, 16);
  RegisterProperty('TaxBase2', 'СтАкцизАдвал', 'Н', 'Ставка налога акцизов адвалорных Графа 17', 1, 16);
  RegisterProperty('VatRate', 'СтНДС', 'О', 'Ставка налога (НДС) Графа 18', 1, 16);
  RegisterProperty('Excise', 'СумАкциз', 'Н', 'Сумма налога (акциз) Графа 19', 1, 16);
  RegisterProperty('Vat', 'СумНДС', 'Н', 'Сумма налога (НДС) Графа 20', 1, 16);
  RegisterProperty('ExciseFlag', 'ПрОсвАкциз', 'ОК', 'Признак освобождения от уплаты налога (акцизы)', 1, 1);
  RegisterProperty('VatFlag', 'ПрОсвНДС', 'ОК', 'Признак освобождения от уплаты налога (НДС)', 1, 1);
  RegisterProperty('ProductDetailDocs', 'СвТСД', 'ОМ', 'Сведения о товаросопроводительных документах', -1, -1);
end;

procedure TProductDetail.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FProductDetailDocs:=TTransferDocs.Create;
end;

destructor TProductDetail.Destroy;
begin
  FreeAndNil(FProductDetailDocs);
  inherited Destroy;
end;

{ TTransferDoc }

procedure TTransferDoc.SetTransferDocDate(AValue: string);
begin
  if FTransferDocDate=AValue then Exit;
  FTransferDocDate:=AValue;
  ModifiedProperty('TransferDocDate');
end;

procedure TTransferDoc.SetTransferDocNumber(AValue: string);
begin
  if FTransferDocNumber=AValue then Exit;
  FTransferDocNumber:=AValue;
  ModifiedProperty('TransferDocNumber');
end;

procedure TTransferDoc.InternalRegisterPropertys;
begin
  RegisterProperty('TransferDocNumber', 'СерНомТСД', 'О', 'Серия, номер транспортного (товаросопроводительного) документа Графа 9', 1, 50);
  RegisterProperty('TransferDocDate', 'ДатаТСД', 'О', 'Дата транспортного (товаросопроводительного) документа Графа 10', 10, 10);
end;

procedure TTransferDoc.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TTransferDoc.Destroy;
begin
  inherited Destroy;
end;

{ TTransferDocs }

function TTransferDocs.GetItem(AIndex: Integer): TTransferDoc;
begin
  Result:=TTransferDoc(InternalGetItem(AIndex));
end;

constructor TTransferDocs.Create;
begin
  inherited Create(TTransferDoc)
end;

function TTransferDocs.CreateChild: TTransferDoc;
begin
  Result:=TTransferDoc(InternalAddObject);
end;

{ TProductDetails }

function TProductDetails.GetItem(AIndex: Integer): TProductDetail;
begin
  Result:=TProductDetail(InternalGetItem(AIndex));
end;

constructor TProductDetails.Create;
begin
  inherited Create(TProductDetail)
end;

function TProductDetails.CreateChild: TProductDetail;
begin
  Result:=TProductDetail(InternalAddObject);
end;

{ TCommissionContractInfo }

procedure TCommissionContractInfo.SetAdress(AValue: string);
begin
  if FAdress=AValue then Exit;
  FAdress:=AValue;
  ModifiedProperty('Adress');
end;

procedure TCommissionContractInfo.SetBaikonurFlag(AValue: string);
begin
  if FBaikonurFlag=AValue then Exit;
  FBaikonurFlag:=AValue;
  ModifiedProperty('BaikonurFlag');
end;

procedure TCommissionContractInfo.SetCountryCode(AValue: string);
begin
  if FCountryCode=AValue then Exit;
  FCountryCode:=AValue;
  ModifiedProperty('CountryCode');
end;

procedure TCommissionContractInfo.SetIdentificationCode(AValue: string);
begin
  if FIdentificationCode=AValue then Exit;
  FIdentificationCode:=AValue;
  ModifiedProperty('IdentificationCode');
end;

procedure TCommissionContractInfo.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
  ModifiedProperty('Name');
end;

procedure TCommissionContractInfo.InternalRegisterPropertys;
begin
  RegisterProperty('BaikonurFlag', 'ПрБкнрОрг', 'Н' , 'Признак нахождения российского посредника в г.Байконур', 1, 1);
  RegisterProperty('IdentificationCode', 'ИдНомОрг', 'Н' , 'Идентификационный код (номер) посредника Раздел 1 стр.06', 1, 50);
  RegisterProperty('Name', 'НаимОрг', 'О' , 'Наименование организации (ФИО индивидуального предпринимателя) посредника Раздел 1 стр.06', 1, 400);
  RegisterProperty('CountryCode', 'КодСтранОрг', 'ОК' , 'Код страны посредника Раздел 1 стр.06', 3, 3);
  RegisterProperty('Adress', 'АдресОрг', 'О' , 'Адрес местонахождения (жительства) посредника Раздел 1 стр.06', 1, 200);
  RegisterProperty('ContractInfo', 'СвКонтракт2', 'О' , 'Сведения о контракте (договоре) Раздел 1 стр.07', -1, -1);
end;

procedure TCommissionContractInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FContractInfo:=TContractInfo.Create;
end;

destructor TCommissionContractInfo.Destroy;
begin
  FreeAndNil(FContractInfo);
  inherited Destroy;
end;

{ TSellerContractInfo }

procedure TSellerContractInfo.SetBuyerAdress(AValue: string);
begin
  if FBuyerAdress=AValue then Exit;
  FBuyerAdress:=AValue;
  ModifiedProperty('BuyerAdress');
end;

procedure TSellerContractInfo.SetBuyerBaikonurFlag(AValue: string);
begin
  if FBuyerBaikonurFlag=AValue then Exit;
  FBuyerBaikonurFlag:=AValue;
  ModifiedProperty('BuyerBaikonurFlag');
end;

procedure TSellerContractInfo.SetBuyerCountryCode(AValue: string);
begin
  if FBuyerCountryCode=AValue then Exit;
  FBuyerCountryCode:=AValue;
  ModifiedProperty('BuyerCountryCode');
end;

procedure TSellerContractInfo.SetBuyerINN(AValue: string);
begin
  if FBuyerINN=AValue then Exit;
  FBuyerINN:=AValue;
  ModifiedProperty('BuyerINN');
end;

procedure TSellerContractInfo.SetBuyerName(AValue: string);
begin
  if FBuyerName=AValue then Exit;
  FBuyerName:=AValue;
  ModifiedProperty('BuyerName');
end;

procedure TSellerContractInfo.SetSellerAdress(AValue: string);
begin
  if FSellerAdress=AValue then Exit;
  FSellerAdress:=AValue;
  ModifiedProperty('SellerAdress');
end;

procedure TSellerContractInfo.SetSellerBaikonurFlag(AValue: string);
begin
  if FSellerBaikonurFlag=AValue then Exit;
  FSellerBaikonurFlag:=AValue;
  ModifiedProperty('SellerBaikonurFlag');
end;

procedure TSellerContractInfo.SetSellerCountryCode(AValue: string);
begin
  if FSellerCountryCode=AValue then Exit;
  FSellerCountryCode:=AValue;
  ModifiedProperty('SellerCountryCode');
end;

procedure TSellerContractInfo.SetSellerIdentificationCode(AValue: string);
begin
  if FSellerIdentificationCode=AValue then Exit;
  FSellerIdentificationCode:=AValue;
  ModifiedProperty('SellerIdentificationCode');
end;

procedure TSellerContractInfo.SetSellerName(AValue: string);
begin
  if FSellerName=AValue then Exit;
  FSellerName:=AValue;
  ModifiedProperty('SellerName');
end;

procedure TSellerContractInfo.SetSellerOrgType(AValue: string);
begin
  if FSellerOrgType=AValue then Exit;
  FSellerOrgType:=AValue;
  ModifiedProperty('SellerOrgType');
end;

procedure TSellerContractInfo.InternalRegisterPropertys;
begin
  RegisterProperty('SellerBaikonurFlag', 'ПрБкнрПродР1', 'Н', 'Признак нахождения российского продавца в г.Байконур', 1, 1);
  RegisterProperty('SellerIdentificationCode', 'ИдНомПродР1', 'Н', 'Идентификационный код (номер) продавца Раздел 1 стр. 01', 1, 50);
  RegisterProperty('SellerOrgType', 'ПрПродФЛ', 'ОК', 'Признак продавца – физического лица (не индивидуального предпринимателя)', 1, 1);
  RegisterProperty('SellerName', 'НаимПродР1', 'О', 'Полное наименование (ФИО) продавца Раздел 1 стр. 01', 1, 400);
  RegisterProperty('SellerCountryCode', 'КодСтранПродР1', 'ОК', 'Код страны продавца Раздел 1 стр. 03', 3, 3);
  RegisterProperty('SellerAdress', 'АдресПродР1', 'О', 'Адрес местонахождения (жительства) продавца Раздел 1 стр. 03', 1, 200);
  RegisterProperty('BuyerBaikonurFlag', 'ПрБкнрПокР1', 'Н', 'Признак нахождения российского покупателя в г.Байконур', 1, 1);
  RegisterProperty('BuyerINN', 'ИдНомПокР1', 'О', 'ИНН покупателя Раздел 1 стр. 02', 10, 12);
  RegisterProperty('BuyerName', 'НаимПокР1', 'О', 'Полное наименование (ФИО) покупателя Раздел 1 стр. 02', 1, 400);
  RegisterProperty('BuyerCountryCode', 'КодСтранПокР1', 'ОК', 'Код страны покупателя Раздел 1 стр. 04', 3, 3);
  RegisterProperty('BuyerAdress', 'АдресПокР1', 'О', 'Адрес местонахождения (жительства) покупателя Раздел 1 стр. 04', 1, 200);
  RegisterProperty('ContractInfo', 'СвКонтр1', 'О', 'Сведения о контракте (договоре) Раздел 1 стр. 05', -1, -1);
end;

procedure TSellerContractInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FContractInfo:=TContractInfo.Create;
end;

destructor TSellerContractInfo.Destroy;
begin
  FreeAndNil(FContractInfo);
  inherited Destroy;
end;

{ TSpecificationsInformations }

function TSpecificationsInformations.GetItem(AIndex: Integer
  ): TSpecificationsInformation;
begin
  Result:=TSpecificationsInformation(InternalGetItem(AIndex));
end;

constructor TSpecificationsInformations.Create;
begin
  inherited Create(TSpecificationsInformation)
end;

function TSpecificationsInformations.CreateChild: TSpecificationsInformation;
begin
  Result:=InternalAddObject as TSpecificationsInformation;
end;

{ TContractInfo }

procedure TContractInfo.SetContractDate(AValue: string);
begin
  if FContractDate=AValue then Exit;
  FContractDate:=AValue;
  ModifiedProperty('ContractDate');
end;

procedure TContractInfo.SetContractNumber(AValue: string);
begin
  if FContractNumber=AValue then Exit;
  FContractNumber:=AValue;
  ModifiedProperty('ContractNumber');
end;

procedure TContractInfo.InternalRegisterPropertys;
begin
  RegisterProperty('ContractNumber', 'НомКонтр', 'О', 'Номер контракта', 1, 50);
  RegisterProperty('ContractDate', 'ДатаКонтр', 'О', 'Дата контракта', 10, 10);
  RegisterProperty('SpecificationsInformation', 'СвСпециф', 'НМ', 'Сведения спецификаций', -1, -1);
end;

procedure TContractInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FSpecificationsInformation:=TSpecificationsInformations.Create;
end;

destructor TContractInfo.Destroy;
begin
  FreeAndNil(FSpecificationsInformation);
  inherited Destroy;
end;

{ TSpecificationsInformation }

procedure TSpecificationsInformation.SetOrderNumber(AValue: string);
begin
  if FOrderNumber=AValue then Exit;
  FOrderNumber:=AValue;
  ModifiedProperty('OrderNumber');
end;

procedure TSpecificationsInformation.SetSpecificationDate(AValue: string);
begin
  if FSpecificationDate=AValue then Exit;
  FSpecificationDate:=AValue;
  ModifiedProperty('SpecificationDate');
end;

procedure TSpecificationsInformation.SetSpecificationNumber(AValue: string);
begin
  if FSpecificationNumber=AValue then Exit;
  FSpecificationNumber:=AValue;
  ModifiedProperty('SpecificationNumber');
end;

procedure TSpecificationsInformation.InternalRegisterPropertys;
begin
  RegisterProperty('OrderNumber', 'НомПСпециф', 'О', 'Номер по порядку', 0, 5);
  RegisterProperty('SpecificationNumber', 'НомСпециф', 'О', 'Номер спецификации', 1, 50);
  RegisterProperty('SpecificationDate', 'ДатаСпециф', 'О', 'Дата спецификации', 10, 10);
end;

procedure TSpecificationsInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TSpecificationsInformation.Destroy;
begin
  inherited Destroy;
end;

{ TDeclarationInfo }

procedure TDeclarationInfo.InternalRegisterPropertys;
begin

end;

procedure TDeclarationInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TDeclarationInfo.Destroy;
begin
  inherited Destroy;
end;

{ TSignerInfo }

procedure TSignerInfo.SetINN(AValue: string);
begin
  if FINN=AValue then Exit;
  FINN:=AValue;
  ModifiedProperty('INN');
end;

procedure TSignerInfo.SetPosition(AValue: string);
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;
  ModifiedProperty('Position');
end;

procedure TSignerInfo.SetSignerType(AValue: string);
begin
  if FSignerType=AValue then Exit;
  FSignerType:=AValue;
  ModifiedProperty('SignerType');
end;

procedure TSignerInfo.InternalRegisterPropertys;
begin
  RegisterProperty('SignerType', 'ПрПодп', 'ОК', 'Признак лица, подписавшего документ', 1, 1);
  RegisterProperty('Position', 'ДолжнПодп', 'Н', 'Должность лица, подписавшего документ', 0, 128);
  RegisterProperty('INN', 'ИННФЛ', 'Н', 'ИНН физического лица', 12, 12);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
  RegisterProperty('AuthorizedInformation', 'СвПред', 'Н', 'Сведения об уполномоченном представителе', -1, -1);
end;

procedure TSignerInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPerson:=TPerson.Create;
  FAuthorizedInformation:=TAuthorizedInformation.Create;
end;

destructor TSignerInfo.Destroy;
begin
  FreeAndNil(FPerson);
  FreeAndNil(FAuthorizedInformation);
  inherited Destroy;
end;

{ TAuthorizedInformation }

procedure TAuthorizedInformation.SetDocumentDate(AValue: string);
begin
  if FDocumentDate=AValue then Exit;
  FDocumentDate:=AValue;
  ModifiedProperty('DocumentDate');
end;

procedure TAuthorizedInformation.SetDocumentName(AValue: string);
begin
  if FDocumentName=AValue then Exit;
  FDocumentName:=AValue;
  ModifiedProperty('DocumentName');
end;

procedure TAuthorizedInformation.SetDocumentNumber(AValue: string);
begin
  if FDocumentNumber=AValue then Exit;
  FDocumentNumber:=AValue;
  ModifiedProperty('DocumentNumber');
end;

procedure TAuthorizedInformation.InternalRegisterPropertys;
begin
  RegisterProperty('DocumentName', 'НаимДов', 'О', 'Наименование документа (доверенности, приказа), подтверждающего полномочия представителя', 1, 120);
  RegisterProperty('DocumentNumber', 'НомерДов', 'О', 'Номер документа', 1, 50);
  RegisterProperty('DocumentDate', 'ДатаДов', 'О', 'Дата документа', 10, 10);
end;

procedure TAuthorizedInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TAuthorizedInformation.Destroy;
begin
  inherited Destroy;
end;

{ TPerson }

procedure TPerson.SetFirstName(AValue: string);
begin
  if FFirstName=AValue then Exit;
  FFirstName:=AValue;
  ModifiedProperty('FirstName');
end;

procedure TPerson.SetPatronymic(AValue: string);
begin
  if FPatronymic=AValue then Exit;
  FPatronymic:=AValue;
  ModifiedProperty('Patronymic');
end;

procedure TPerson.SetSurname(AValue: string);
begin
  if FSurname=AValue then Exit;
  FSurname:=AValue;
  ModifiedProperty('Surname');
end;

procedure TPerson.InternalRegisterPropertys;
begin
  RegisterProperty('Surname', 'Фамилия', 'О', 'Фамилия', 1, 60);
  RegisterProperty('FirstName', 'Имя', 'О', 'Имя', 1, 60);
  RegisterProperty('Patronymic', 'Отчество', 'Н', 'Отчество', 1, 60);
end;

procedure TPerson.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TPerson.Destroy;
begin
  inherited Destroy;
end;

{ TPhysicalPersonEntity }

procedure TPhysicalPersonEntity.SetINN(AValue: string);
begin
  if FINN=AValue then Exit;
  FINN:=AValue;
  ModifiedProperty('INN');
end;

procedure TPhysicalPersonEntity.InternalRegisterPropertys;
begin
  RegisterProperty('INN', 'ИННФЛ', 'Н', 'ИНН физического лица', 12, 12);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
end;

procedure TPhysicalPersonEntity.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPerson:=TPerson.Create;
end;

destructor TPhysicalPersonEntity.Destroy;
begin
  FreeAndNil(FPerson);
  inherited Destroy;
end;

{ TLegalEntityInformation }

procedure TLegalEntityInformation.SetFullName(AValue: string);
begin
  if FFullName=AValue then Exit;
  FFullName:=AValue;
  ModifiedProperty('FullName');
end;

procedure TLegalEntityInformation.SetINN(AValue: string);
begin
  if FINN=AValue then Exit;
  FINN:=AValue;
  ModifiedProperty('INN');
end;

procedure TLegalEntityInformation.SetKPP(AValue: string);
begin
  if FKPP=AValue then Exit;
  FKPP:=AValue;
  ModifiedProperty('KPP');
end;

procedure TLegalEntityInformation.InternalRegisterPropertys;
begin
  RegisterProperty('FullName', 'НаимОрг', 'О', 'Наименование организации', 1, 400);
  RegisterProperty('INN', 'ИННЮЛ', 'О', 'ИНН организации', 10, 10);
  RegisterProperty('KPP', 'КПП', 'О', 'КПП', 9, 9);
end;

procedure TLegalEntityInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TLegalEntityInformation.Destroy;
begin
  inherited Destroy;
end;

{ TSenderInfo }

procedure TSenderInfo.InternalRegisterPropertys;
begin
  RegisterProperty('LegalEntityInformation', 'ОтпрЮЛ', 'О', 'Отправитель – организация', -1, -1);
  RegisterProperty('PhysicalPerson', 'ОтпрФЛ', 'О', 'Отправитель - физическое лицо', -1, -1);
end;

procedure TSenderInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FLegalEntityInformation:=TLegalEntityInformation.Create;
  FPhysicalPerson:=TPhysicalPersonEntity.Create;
end;

destructor TSenderInfo.Destroy;
begin
  FreeAndNil(FLegalEntityInformation);
  FreeAndNil(FPhysicalPerson);
  inherited Destroy;
end;

{ TImportGoodsAndIndirectTaxesDocument }

procedure TImportGoodsAndIndirectTaxesDocument.SetDocumentDate(AValue: string);
begin
  if FDocumentDate=AValue then Exit;
  FDocumentDate:=AValue;
end;

procedure TImportGoodsAndIndirectTaxesDocument.SetKND(AValue: string);
begin
  if FKND=AValue then Exit;
  FKND:=AValue;
end;

procedure TImportGoodsAndIndirectTaxesDocument.InternalRegisterPropertys;
begin
  RegisterProperty('KND', 'КНД', 'О', 'Код формы по КНД', 7, 7);
  RegisterProperty('DocumentDate', 'ДатаДок', 'О', 'Дата формирования документа', 10, 10);
  RegisterProperty('SenderInfo', 'СвОтпр', 'О', 'Сведения об отправителе документа', -1, -1);
  RegisterProperty('SignerInfo', 'Подписант', 'О', 'Сведения о лице, подписавшем документ', -1, -1);
  RegisterProperty('DeclarationInfo', 'СвЗвл', 'О', 'Сведения из заявления', -1, -1);
(*
  Сведения о договорах (контрактах) приложения к Заявлению
  СвКонтрПр
  С
   
  НМ
  Состав элемента представлен в табл. 4.11
*)
end;

procedure TImportGoodsAndIndirectTaxesDocument.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FSenderInfo:=TSenderInfo.Create;
  FSignerInfo:=TSignerInfo.Create;
  FDeclarationInfo:=TDeclarationInfo.Create;
end;

destructor TImportGoodsAndIndirectTaxesDocument.Destroy;
begin
  FreeAndNil(FSenderInfo);
  FreeAndNil(FSignerInfo);
  FreeAndNil(FDeclarationInfo);
  inherited Destroy;
end;

end.

