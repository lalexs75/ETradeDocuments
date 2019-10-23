{ interface library for FPC and Lazarus

  Copyright (C) 2019 Lagunov Aleksey alexs75@yandex.ru

  Генерация xml файлов для электронного документооборота

  Структуры данных базируются на основании "Приказ ФНС РФ от 19.12.2018 N ММВ-7-15/820@"

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

unit InvoiceItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, AdditionalInfo;

type

  { TExciseSum }

  TExciseSum = class(TXmlSerializationObject)   //%Таблица 5.45
  private
    FSum: string;
    FWithout: string;
    procedure SetSum(AValue: string);
    procedure SetWithout(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Sum:string read FSum write SetSum;
    property Without:string read FWithout write SetWithout;
  end;

  { TVatSum }

  TVatSum = class(TXmlSerializationObject)   //%Таблица 5.46
  private
    FVatValue: string;
    FVatValueDef: string;
    FWithoutVat: string;
    procedure SetVatValue(AValue: string);
    procedure SetVatValueDef(AValue: string);
    procedure SetWithoutVat(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property VatValue:string read FVatValue write SetVatValue;
    property WithoutVat:string read FWithoutVat write SetWithoutVat;
    property VatValueDef:string read FVatValueDef write SetVatValueDef;
  end;

  { TCustomsDeclaration }

  TCustomsDeclaration = class(TXmlSerializationObject)   //%Таблица 5.15
  private
    FCountryCode: string;
    FCountryCodeDef: string;
    FDeclarationNumber: string;
    procedure SetCountryCode(AValue: string);
    procedure SetCountryCodeDef(AValue: string);
    procedure SetDeclarationNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property CountryCode:string read FCountryCode write SetCountryCode;
    property CountryCodeDef:string read FCountryCodeDef write SetCountryCodeDef;
    property DeclarationNumber:string read FDeclarationNumber write SetDeclarationNumber;
  end;

  { TCustomsDeclarationList }

  TCustomsDeclarationList = class(TXmlSerializationObjectList) //%Таблица 5.15
  private
    function GetItem(AIndex: Integer): TCustomsDeclaration; inline;
  public
    constructor Create;
    function CreateChild:TCustomsDeclaration;
    property Item[AIndex:Integer]:TCustomsDeclaration read GetItem; default;
  end;

  { TTraceabilityInformation }

  TTraceabilityInformation = class(TXmlSerializationObject)   //%Таблица 5.17
  private
    FAdditionalInfo: string;
    FBatchNumber: string;
    FQuantity: string;
    FUnitCode: string;
    FUnitName: string;
    procedure SetAdditionalInfo(AValue: string);
    procedure SetBatchNumber(AValue: string);
    procedure SetQuantity(AValue: string);
    procedure SetUnitCode(AValue: string);
    procedure SetUnitName(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property BatchNumber:string read FBatchNumber write SetBatchNumber;
    property UnitCode:string read FUnitCode write SetUnitCode;
    property UnitName:string read FUnitName write SetUnitName;
    property Quantity:string read FQuantity write SetQuantity;
    property AdditionalInfo:string read FAdditionalInfo write SetAdditionalInfo;
  end;

  { TTraceabilityInformationList }

  TTraceabilityInformationList = class(TXmlSerializationObjectList) //%Таблица 5.17
  private
    function GetItem(AIndex: Integer): TTraceabilityInformation; inline;
  public
    constructor Create;
    function CreateChild:TTraceabilityInformation;
    property Item[AIndex:Integer]:TTraceabilityInformation read GetItem; default;
  end;

  { TProductIdentificationNumber }

  TProductIdentificationNumber = class(TXmlSerializationObject)   //%Таблица 5.18
  private
    FCheckMark: TStrings;
    FPackagingIdentificationNumber: string;
    FSecondaryPackagingIdentificationNumber: TStrings;
    procedure SetPackagingIdentificationNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property PackagingIdentificationNumber:string read FPackagingIdentificationNumber write SetPackagingIdentificationNumber;
    property CheckMark:TStrings read FCheckMark;
    property SecondaryPackagingIdentificationNumber:TStrings read FSecondaryPackagingIdentificationNumber;
  end;

  { TProductIdentificationNumberList }

  TProductIdentificationNumberList = class(TXmlSerializationObjectList) //%Таблица 5.18
  private
    function GetItem(AIndex: Integer): TProductIdentificationNumber; inline;
  public
    constructor Create;
    function CreateChild:TProductIdentificationNumber;
    property Item[AIndex:Integer]:TProductIdentificationNumber read GetItem; default;
  end;

  { TInvoiceItemAdditional }

  TInvoiceItemAdditional = class(TXmlSerializationObject)   //%Таблица 5.16
  private
    FAdditionalProperty: string;
    FCountryNameShort: string;
    FItemArticleNumber: string;
    FItemCatalogCode: string;
    FItemCharacteristic: string;
    FItemMark: string;
    FItemSort: string;
    FItemToRelease: string;
    FItemTypeCode: string;
    FItemVendorCode: string;
    FProductIdentificationNumber: TProductIdentificationNumberList;
    FTraceabilityInformation: TTraceabilityInformationList;
    FUnitName: string;
    procedure SetAdditionalProperty(AValue: string);
    procedure SetCountryNameShort(AValue: string);
    procedure SetItemArticleNumber(AValue: string);
    procedure SetItemCatalogCode(AValue: string);
    procedure SetItemCharacteristic(AValue: string);
    procedure SetItemMark(AValue: string);
    procedure SetItemSort(AValue: string);
    procedure SetItemToRelease(AValue: string);
    procedure SetItemTypeCode(AValue: string);
    procedure SetItemVendorCode(AValue: string);
    procedure SetUnitName(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ItemMark:string read FItemMark write SetItemMark;
    property AdditionalProperty:string read FAdditionalProperty write SetAdditionalProperty;
    property UnitName:string read FUnitName write SetUnitName;
    property CountryNameShort:string read FCountryNameShort write SetCountryNameShort;
    property ItemToRelease:string read FItemToRelease write SetItemToRelease;
    property ItemCharacteristic:string read FItemCharacteristic write SetItemCharacteristic;
    property ItemSort:string read FItemSort write SetItemSort;
    property ItemArticleNumber:string read FItemArticleNumber write SetItemArticleNumber;
    property ItemVendorCode:string read FItemVendorCode write SetItemVendorCode;
    property ItemCatalogCode:string read FItemCatalogCode write SetItemCatalogCode;
    property ItemTypeCode:string read FItemTypeCode write SetItemTypeCode;
    property TraceabilityInformation:TTraceabilityInformationList read FTraceabilityInformation;
    property ProductIdentificationNumber:TProductIdentificationNumberList read FProductIdentificationNumber;
  end;

  { TInvoiceItem }

  TInvoiceItem = class(TXmlSerializationObject)   //%Таблица 5.14
  private
    FAdditionalInfo: TTextInfoList;
    FCustomsDeclaration: TCustomsDeclarationList;
    FExcise: TExciseSum;
    FInvoiceItemAdditional: TInvoiceItemAdditional;
    FLineNumber: Integer;
    FPrice: string;
    FProduct: string;
    FQuantity: string;
    FSubtotal: string;
    FSubtotalDef: string;
    FSubtotalWithVatExcluded: string;
    FTaxRate: string;
    FUnitCode: string;
    FUnitCodeDef: string;
    FVat: TVatSum;
    procedure SetLineNumber(AValue: Integer);
    procedure SetPrice(AValue: string);
    procedure SetProduct(AValue: string);
    procedure SetQuantity(AValue: string);
    procedure SetSubtotal(AValue: string);
    procedure SetSubtotalDef(AValue: string);
    procedure SetSubtotalWithVatExcluded(AValue: string);
    procedure SetTaxRate(AValue: string);
    procedure SetUnitCode(AValue: string);
    procedure SetUnitCodeDef(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property LineNumber:Integer read FLineNumber write SetLineNumber;
    property Product:string read FProduct write SetProduct;
    property UnitCode:string read FUnitCode write SetUnitCode;
    property UnitCodeDef:string read FUnitCodeDef write SetUnitCodeDef;
    property Quantity:string read FQuantity write SetQuantity;
    property Price:string read FPrice write SetPrice;
    property SubtotalWithVatExcluded:string read FSubtotalWithVatExcluded write SetSubtotalWithVatExcluded;
    property TaxRate:string read FTaxRate write SetTaxRate;
    property Subtotal:string read FSubtotal write SetSubtotal;
    property SubtotalDef:string read FSubtotalDef write SetSubtotalDef;
    property Excise:TExciseSum read FExcise;
    property Vat:TVatSum read FVat;
    property CustomsDeclaration:TCustomsDeclarationList read FCustomsDeclaration;
    property InvoiceItemAdditional:TInvoiceItemAdditional read FInvoiceItemAdditional;
    property AdditionalInfo:TTextInfoList read FAdditionalInfo;
  end;

  { TInvoiceItemList }

  TInvoiceItemList = class(TXmlSerializationObjectList) //%Таблица 5.33
  private
    function GetItem(AIndex: Integer): TInvoiceItem; inline;
  public
    constructor Create;
    function CreateChild:TInvoiceItem;
    property Item[AIndex:Integer]:TInvoiceItem read GetItem; default;
  end;

  { TTotalForPay }

  TTotalForPay = class(TXmlSerializationObject)   //%Таблица 5.19
  private
    FTotal: string;
    FTotalDef: string;
    FTotalNet: string;
    FTotalWithVatExcluded: string;
    FVat: TVatSum;
    procedure SetTotal(AValue: string);
    procedure SetTotalDef(AValue: string);
    procedure SetTotalNet(AValue: string);
    procedure SetTotalWithVatExcluded(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property TotalWithVatExcluded:string read FTotalWithVatExcluded write SetTotalWithVatExcluded;
    property Total:string read FTotal write SetTotal;
    property TotalDef:string read FTotalDef write SetTotalDef;
    property Vat:TVatSum read FVat;
    property TotalNet:string read FTotalNet write SetTotalNet;
  end;

  { TInvoiceItems }

  TInvoiceItems = class(TXmlSerializationObject)   //%Таблица 5.13
  private
    FInvoiceItemList: TInvoiceItemList;
    FTotalForPay: TTotalForPay;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property InvoiceItemList:TInvoiceItemList read FInvoiceItemList;
    property TotalForPay:TTotalForPay read FTotalForPay;
  end;


implementation

{ TTotalForPay }

procedure TTotalForPay.SetTotal(AValue: string);
begin
  if FTotal=AValue then Exit;
  FTotal:=AValue;
  ModifiedProperty('Total');
end;

procedure TTotalForPay.SetTotalDef(AValue: string);
begin
  if FTotalDef=AValue then Exit;
  FTotalDef:=AValue;
  ModifiedProperty('TotalDef');
end;

procedure TTotalForPay.SetTotalNet(AValue: string);
begin
  if FTotalNet=AValue then Exit;
  FTotalNet:=AValue;
  ModifiedProperty('TotalNet');
end;

procedure TTotalForPay.SetTotalWithVatExcluded(AValue: string);
begin
  if FTotalWithVatExcluded=AValue then Exit;
  FTotalWithVatExcluded:=AValue;
  ModifiedProperty('TotalWithVatExcluded');
end;

procedure TTotalForPay.InternalRegisterPropertys;
begin
  RegisterProperty('TotalWithVatExcluded', 'СтТовБезНДСВсего', 'Н', 'Всего к оплате, Стоимость товаров (работ, услуг), имущественных прав без налога - всего (строка "Всего к оплате"/графа 5 счета-фактуры)', 1, 19);
  RegisterProperty('Total', 'СтТовУчНалВсего', 'Н', 'Всего к оплате, Стоимость товаров (работ, услуг), имущественных прав с налогом - всего (строка "Всего к оплате"/графа 9 счета-фактуры)', 1, 19);
  RegisterProperty('TotalDef', 'ДефСтТовУчНалВсего', 'Н', 'Всего к оплате, Стоимость товаров (работ, услуг), имущественных прав с налогом - всего (строка "Всего к оплате"/графа 9 счета-фактуры) при отсутствии показателя', 1, 1);
  RegisterProperty('Vat', 'СумНалВсего', 'СО', 'Всего к оплате, Сумма налога, предъявляемая покупателю (строка "Всего к оплате"/графа 8 счета-фактуры)', -1, -1);
  RegisterProperty('TotalNet', 'КолНеттоВс', 'НП', 'Количество (масса нетто/количество) - всего по документу', 1, 26, 'НеттоВс');
end;

procedure TTotalForPay.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FVat:=TVatSum.Create;
end;

destructor TTotalForPay.Destroy;
begin
  FreeAndNil(FVat);
  inherited Destroy;
end;

{ TProductIdentificationNumber }

procedure TProductIdentificationNumber.SetPackagingIdentificationNumber(
  AValue: string);
begin
  if FPackagingIdentificationNumber=AValue then Exit;
  FPackagingIdentificationNumber:=AValue;
  ModifiedProperty('PackagingIdentificationNumber');
end;

procedure TProductIdentificationNumber.InternalRegisterPropertys;
begin
  RegisterProperty('PackagingIdentificationNumber', 'ИдентТрансУпак', 'Н', 'Уникальный идентификатор транспортной упаковки', 1, 255);
  RegisterProperty('CheckMark', 'КИЗ', 'ПНМ', 'Контрольный идентификационный знак', -1, -1);
  RegisterProperty('SecondaryPackagingIdentificationNumber', 'НомУпак', 'ПНМ', 'Уникальный идентификатор вторичной (потребительской)/третичной (заводской, транспортной) упаковки', -1, -1);
end;

procedure TProductIdentificationNumber.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FCheckMark:=TStringList.Create;
  FSecondaryPackagingIdentificationNumber:=TStringList.Create;
end;

destructor TProductIdentificationNumber.Destroy;
begin
  FreeAndNil(FCheckMark);
  FreeAndNil(FSecondaryPackagingIdentificationNumber);
  inherited Destroy;
end;

{ TProductIdentificationNumberList }

function TProductIdentificationNumberList.GetItem(AIndex: Integer
  ): TProductIdentificationNumber;
begin
  Result:=TProductIdentificationNumber(InternalGetItem(AIndex));
end;

constructor TProductIdentificationNumberList.Create;
begin
  inherited Create(TProductIdentificationNumber);
end;

function TProductIdentificationNumberList.CreateChild: TProductIdentificationNumber;
begin
  Result:=InternalAddObject as TProductIdentificationNumber;
end;

{ TTraceabilityInformation }

procedure TTraceabilityInformation.SetAdditionalInfo(AValue: string);
begin
  if FAdditionalInfo=AValue then Exit;
  FAdditionalInfo:=AValue;
  ModifiedProperty('AdditionalInfo');
end;

procedure TTraceabilityInformation.SetBatchNumber(AValue: string);
begin
  if FBatchNumber=AValue then Exit;
  FBatchNumber:=AValue;
  ModifiedProperty('BatchNumber');
end;

procedure TTraceabilityInformation.SetQuantity(AValue: string);
begin
  if FQuantity=AValue then Exit;
  FQuantity:=AValue;
  ModifiedProperty('Quantity');
end;

procedure TTraceabilityInformation.SetUnitCode(AValue: string);
begin
  if FUnitCode=AValue then Exit;
  FUnitCode:=AValue;
  ModifiedProperty('UnitCode');
end;

procedure TTraceabilityInformation.SetUnitName(AValue: string);
begin
  if FUnitName=AValue then Exit;
  FUnitName:=AValue;
  ModifiedProperty('UnitName');
end;

procedure TTraceabilityInformation.InternalRegisterPropertys;
begin
  RegisterProperty('BatchNumber', 'НомТовПрослеж', 'О', 'Регистрационный номер партии товаров', 1, 29);
  RegisterProperty('UnitCode', 'ЕдИзмПрослеж', 'ОК', 'Единица количественного учета товара, используемая в целях осуществления прослеживаемости', 3, 4);
  RegisterProperty('UnitName', 'НаимЕдИзмПрослеж', 'ОК', 'Наименование единицы количественного учета товара, используемой в целях осуществления прослеживаемости', 1, 255);
  RegisterProperty('Quantity', 'КолВЕдПрослеж', 'Количество товара в единицах измерения прослеживаемого товара', 'О', 1, 26);
  RegisterProperty('AdditionalInfo', 'ДопПрослеж', 'Н', 'Дополнительный показатель для идентификации товаров, подлежащих прослеживаемости', 1, 255);
end;

procedure TTraceabilityInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TTraceabilityInformation.Destroy;
begin
  inherited Destroy;
end;

{ TTraceabilityInformationList }

function TTraceabilityInformationList.GetItem(AIndex: Integer
  ): TTraceabilityInformation;
begin
  Result:=TTraceabilityInformation(InternalGetItem(AIndex));
end;

constructor TTraceabilityInformationList.Create;
begin
  inherited Create(TTraceabilityInformation);
end;

function TTraceabilityInformationList.CreateChild: TTraceabilityInformation;
begin
  Result:=InternalAddObject as TTraceabilityInformation;
end;

{ TInvoiceItemAdditional }

procedure TInvoiceItemAdditional.SetAdditionalProperty(AValue: string);
begin
  if FAdditionalProperty=AValue then Exit;
  FAdditionalProperty:=AValue;
  ModifiedProperty('AdditionalProperty');
end;

procedure TInvoiceItemAdditional.SetCountryNameShort(AValue: string);
begin
  if FCountryNameShort=AValue then Exit;
  FCountryNameShort:=AValue;
  ModifiedProperty('CountryNameShort');
end;

procedure TInvoiceItemAdditional.SetItemArticleNumber(AValue: string);
begin
  if FItemArticleNumber=AValue then Exit;
  FItemArticleNumber:=AValue;
  ModifiedProperty('ItemArticleNumber');
end;

procedure TInvoiceItemAdditional.SetItemCatalogCode(AValue: string);
begin
  if FItemCatalogCode=AValue then Exit;
  FItemCatalogCode:=AValue;
  ModifiedProperty('ItemCatalogCode');
end;

procedure TInvoiceItemAdditional.SetItemCharacteristic(AValue: string);
begin
  if FItemCharacteristic=AValue then Exit;
  FItemCharacteristic:=AValue;
  ModifiedProperty('ItemCharacteristic');
end;

procedure TInvoiceItemAdditional.SetItemMark(AValue: string);
begin
  if FItemMark=AValue then Exit;
  FItemMark:=AValue;
  ModifiedProperty('ItemMark');
end;

procedure TInvoiceItemAdditional.SetItemSort(AValue: string);
begin
  if FItemSort=AValue then Exit;
  FItemSort:=AValue;
  ModifiedProperty('ItemSort');
end;

procedure TInvoiceItemAdditional.SetItemToRelease(AValue: string);
begin
  if FItemToRelease=AValue then Exit;
  FItemToRelease:=AValue;
  ModifiedProperty('ItemToRelease');
end;

procedure TInvoiceItemAdditional.SetItemTypeCode(AValue: string);
begin
  if FItemTypeCode=AValue then Exit;
  FItemTypeCode:=AValue;
  ModifiedProperty('ItemTypeCode');
end;

procedure TInvoiceItemAdditional.SetItemVendorCode(AValue: string);
begin
  if FItemVendorCode=AValue then Exit;
  FItemVendorCode:=AValue;
  ModifiedProperty('ItemVendorCode');
end;

procedure TInvoiceItemAdditional.SetUnitName(AValue: string);
begin
  if FUnitName=AValue then Exit;
  FUnitName:=AValue;
  ModifiedProperty('UnitName');
end;

procedure TInvoiceItemAdditional.InternalRegisterPropertys;
begin
  RegisterProperty('ItemMark', 'ПрТовРаб', 'НК', 'Признак Товар/Работа/Услуга/Право/Иное', 1, 1);
  RegisterProperty('AdditionalProperty', 'ДопПризн', 'Н', 'Дополнительная информация о признаке', 1, 4);
  RegisterProperty('UnitName', 'НаимЕдИзм', 'НК', 'Наименование единицы измерения (условное обозначение национальное, графа 2а счета-фактуры)', 1, 255);
  RegisterProperty('CountryNameShort', 'КрНаимСтрПр', 'Н', 'Краткое наименование страны происхождения товара (графа 10а счета-фактуры)/страна регистрации производителя товара', 1, 255);
  RegisterProperty('ItemToRelease', 'НадлОтп', 'Н', 'Заказанное количество (количество надлежит отпустить)', 1, 26);
  RegisterProperty('ItemCharacteristic', 'ХарактерТов', 'Н', 'Характеристика/описание товара (в том числе графа 1 счета-фактуры)', 1, 1000);
  RegisterProperty('ItemSort', 'СортТов', 'Н', 'Сорт товара', 1, 10);
  RegisterProperty('ItemArticleNumber', 'АртикулТов', 'Н', 'Артикул товара (в том числе графа 1 счета-фактуры)', 1, 50);
  RegisterProperty('ItemVendorCode', 'КодТов', 'Н', 'Код товара (в том числе графа 1 счета-фактуры)', 1, 100);
  RegisterProperty('ItemCatalogCode', 'КодКат', 'Н', 'Код каталога', 27, 27);
  RegisterProperty('ItemTypeCode', 'КодВидТов', 'НК', 'Код вида товара', 10, 10);
  RegisterProperty('TraceabilityInformation', 'СведПрослеж', 'НМ', 'Сведения о товаре, подлежащем прослеживаемости', -1, -1);
  RegisterProperty('ProductIdentificationNumber', 'НомСредИдентТов', 'НМ', 'Номер средств идентификации товаров', -1, -1);
end;

procedure TInvoiceItemAdditional.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FTraceabilityInformation:=TTraceabilityInformationList.Create;
  FProductIdentificationNumber:=TProductIdentificationNumberList.Create;
end;

destructor TInvoiceItemAdditional.Destroy;
begin
  FreeAndNil(FTraceabilityInformation);
  FreeAndNil(FProductIdentificationNumber);
  inherited Destroy;
end;

{ TCustomsDeclaration }

procedure TCustomsDeclaration.SetCountryCode(AValue: string);
begin
  if FCountryCode=AValue then Exit;
  FCountryCode:=AValue;
  ModifiedProperty('CountryCode');
end;

procedure TCustomsDeclaration.SetCountryCodeDef(AValue: string);
begin
  if FCountryCodeDef=AValue then Exit;
  FCountryCodeDef:=AValue;
  ModifiedProperty('CountryCodeDef');
end;

procedure TCustomsDeclaration.SetDeclarationNumber(AValue: string);
begin
  if FDeclarationNumber=AValue then Exit;
  FDeclarationNumber:=AValue;
  ModifiedProperty('DeclarationNumber');
end;

procedure TCustomsDeclaration.InternalRegisterPropertys;
begin
  RegisterProperty('CountryCode', 'КодПроисх', 'НК', 'Цифровой код страны происхождения товара (Графа 10 счета-фактуры)', 3, 3);
  RegisterProperty('CountryCodeDef', 'ДефКодПроисх', 'Н', 'Цифровой код страны происхождения товара (Графа 10 счета-фактуры; для документа с Функция=СЧФ, выставляемом при получении оплаты, частичной оплаты в счет предстоящих поставок товаров (выполнения работ, оказания услуг), передачи имущественных прав', 1, 1);
  RegisterProperty('DeclarationNumber', 'НомерТД', 'Н', 'Регистрационный номер таможенной декларации (номер декларации на товары; графа 11 счета-фактуры)', 1, 29);
end;

procedure TCustomsDeclaration.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TCustomsDeclaration.Destroy;
begin
  inherited Destroy;
end;

{ TCustomsDeclarationList }

function TCustomsDeclarationList.GetItem(AIndex: Integer): TCustomsDeclaration;
begin
  Result:=TCustomsDeclaration(InternalGetItem(AIndex));
end;

constructor TCustomsDeclarationList.Create;
begin
  inherited Create(TCustomsDeclaration)
end;

function TCustomsDeclarationList.CreateChild: TCustomsDeclaration;
begin
  Result:=InternalAddObject as TCustomsDeclaration;
end;

{ TVatSum }

procedure TVatSum.SetVatValue(AValue: string);
begin
  if FVatValue=AValue then Exit;
  FVatValue:=AValue;
  ModifiedProperty('VatValue');
end;

procedure TVatSum.SetVatValueDef(AValue: string);
begin
  if FVatValueDef=AValue then Exit;
  FVatValueDef:=AValue;
  ModifiedProperty('VatValueDef');
end;

procedure TVatSum.SetWithoutVat(AValue: string);
begin
  if FWithoutVat=AValue then Exit;
  FWithoutVat:=AValue;
  ModifiedProperty('WithoutVat');
end;

procedure TVatSum.InternalRegisterPropertys;
begin
  RegisterProperty('VatValue', 'СумНал', 'ОП', 'Значение', 1, 19);
  RegisterProperty('WithoutVat', 'БезНДС', 'ОП', 'При определении налоговой базы налоговыми агентами - покупателями (получателями) товаров, перечисленных в пункте 8 статьи 161 НК РФ, продавцами может указываться 0 (ноль; визуализируется как прочерк), если иное не предусмотрено правилами заполнения счета-фактуры, применяемого при расчетах по налогу на добавленную стоимость, утвержденными Постановлением N 1137', 1, 18);
  RegisterProperty('VatValueDef', 'ДефНДС', 'ОП', 'Знак прочерка', 1, 1);
end;

procedure TVatSum.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TVatSum.Destroy;
begin
  inherited Destroy;
end;

{ TExciseSum }

procedure TExciseSum.SetSum(AValue: string);
begin
  if FSum=AValue then Exit;
  FSum:=AValue;
  ModifiedProperty('Sum');
end;

procedure TExciseSum.SetWithout(AValue: string);
begin
  if FWithout=AValue then Exit;
  FWithout:=AValue;
  ModifiedProperty('Without');
end;

procedure TExciseSum.InternalRegisterPropertys;
begin
  RegisterProperty('Sum', 'СумАкциз', 'П', 'Сумма акциза', 1, 19);
  RegisterProperty('Without', 'БезАкциз', 'П', 'Без акциза', 10, 10);
end;

procedure TExciseSum.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TExciseSum.Destroy;
begin
  inherited Destroy;
end;

{ TInvoiceItemList }

function TInvoiceItemList.GetItem(AIndex: Integer): TInvoiceItem;
begin
  Result:=TInvoiceItem(InternalGetItem(AIndex));
end;

constructor TInvoiceItemList.Create;
begin
  inherited Create(TInvoiceItem)
end;

function TInvoiceItemList.CreateChild: TInvoiceItem;
begin
  Result:=InternalAddObject as TInvoiceItem;
end;

{ TInvoiceItem }

procedure TInvoiceItem.SetLineNumber(AValue: Integer);
begin
  if FLineNumber=AValue then Exit;
  FLineNumber:=AValue;
  ModifiedProperty('LineNumber');
end;

procedure TInvoiceItem.SetPrice(AValue: string);
begin
  if FPrice=AValue then Exit;
  FPrice:=AValue;
  ModifiedProperty('Price');
end;

procedure TInvoiceItem.SetProduct(AValue: string);
begin
  if FProduct=AValue then Exit;
  FProduct:=AValue;
  ModifiedProperty('Product');
end;

procedure TInvoiceItem.SetQuantity(AValue: string);
begin
  if FQuantity=AValue then Exit;
  FQuantity:=AValue;
  ModifiedProperty('Quantity');
end;

procedure TInvoiceItem.SetSubtotal(AValue: string);
begin
  if FSubtotal=AValue then Exit;
  FSubtotal:=AValue;
  ModifiedProperty('Subtotal');
end;

procedure TInvoiceItem.SetSubtotalDef(AValue: string);
begin
  if FSubtotalDef=AValue then Exit;
  FSubtotalDef:=AValue;
  ModifiedProperty('SubtotalDef');
end;

procedure TInvoiceItem.SetSubtotalWithVatExcluded(AValue: string);
begin
  if FSubtotalWithVatExcluded=AValue then Exit;
  FSubtotalWithVatExcluded:=AValue;
  ModifiedProperty('SubtotalWithVatExcluded');
end;

procedure TInvoiceItem.SetTaxRate(AValue: string);
begin
  if FTaxRate=AValue then Exit;
  FTaxRate:=AValue;
  ModifiedProperty('TaxRate');
end;

procedure TInvoiceItem.SetUnitCode(AValue: string);
begin
  if FUnitCode=AValue then Exit;
  FUnitCode:=AValue;
  ModifiedProperty('UnitCode');
end;

procedure TInvoiceItem.SetUnitCodeDef(AValue: string);
begin
  if FUnitCodeDef=AValue then Exit;
  FUnitCodeDef:=AValue;
  ModifiedProperty('UnitCodeDef');
end;

procedure TInvoiceItem.InternalRegisterPropertys;
begin
  RegisterProperty('LineNumber', 'НомСтр', 'О', 'Номер строки таблицы', 0, 999999);
  RegisterProperty('Product', 'НаимТов', 'Н', 'Наименование товара (описание выполненных работ, оказанных услуг), имущественных прав (в том числе графа 1 счета-фактуры)', 1, 1000);
  RegisterProperty('UnitCode', 'ОКЕИ_Тов', 'О', 'Код единицы измерения (графа 2 счета-фактуры)', 3, 4);
  RegisterProperty('UnitCodeDef', 'ДефОКЕИ_Тов', 'Н', 'Код единицы измерения (графа 2 счета-фактуры при составлении документа с Функция=СЧФ или Функция=СЧФДОП при отсутствии данных и для документа с Функция=СЧФ, выставляемом при получении оплаты, частичной оплаты в счет предстоящих поставок товаров (выполнения работ, оказания услуг), передачи имущественных прав)', 1, 1);
  RegisterProperty('Quantity', 'КолТов', 'Н', 'Количество (объем) (графа 3 счета-фактуры)', 1, 26);
  RegisterProperty('Price', 'ЦенаТов', 'Н', 'Цена (тариф) за единицу измерения (графа 4 счета-фактуры)', 1, 26);
  RegisterProperty('SubtotalWithVatExcluded', 'СтТовБезНДС', 'Н', 'Стоимость товаров (работ, услуг), имущественных прав без налога - всего (графа 5 счета-фактуры)', -1, -1);
  RegisterProperty('TaxRate', 'НалСт', 'ОК', 'Налоговая ставка (графа 7 счета-фактуры)', 1, 35);
  RegisterProperty('Subtotal', 'СтТовУчНал', 'Н', 'Стоимость товаров (работ, услуг), имущественных прав с налогом - всего (графа 9 счета-фактуры)', 1, 19);
  RegisterProperty('SubtotalDef', 'ДефСтТовУчНал', 'Н', 'Стоимость товаров (работ, услуг), имущественных прав с налогом - всего (графа 9 счета-фактуры) при составлении документа с Функция=СЧФ или Функция=СЧФДОП при отсутствии показателя', 1, 1);
  RegisterProperty('Excise', 'Акциз', 'О', 'В том числе сумма акциза (графа 6 счета-фактуры)', -1, -1);
  RegisterProperty('Vat', 'СумНал', 'О', 'Сумма налога, предъявляемая покупателю (графа 8 счета-фактуры)', -1, -1);
  RegisterProperty('CustomsDeclaration', 'СвТД', 'НМ', 'Сведения о таможенной декларации', -1, -1);
  RegisterProperty('InvoiceItemAdditional', 'ДопСведТов', 'Н', 'Дополнительные сведения об отгруженных товарах (выполненных работах, оказанных услугах), переданных имущественных правах', -1, -1);
  RegisterProperty('AdditionalInfo', 'ИнфПолФХЖ2', 'НМ', 'Информационное поле факта хозяйственной жизни 2', -1, -1);
end;

procedure TInvoiceItem.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FExcise:=TExciseSum.Create;
  FVat:=TVatSum.Create;
  FCustomsDeclaration:=TCustomsDeclarationList.Create;
  FInvoiceItemAdditional:=TInvoiceItemAdditional.Create;
  FAdditionalInfo:=TTextInfoList.Create;
end;

destructor TInvoiceItem.Destroy;
begin
  FreeAndNil(FExcise);
  FreeAndNil(FVat);
  FreeAndNil(FCustomsDeclaration);
  FreeAndNil(FInvoiceItemAdditional);
  FreeAndNil(FAdditionalInfo);
  inherited Destroy;
end;

{ TInvoiceItems }

procedure TInvoiceItems.InternalRegisterPropertys;
begin
  RegisterProperty('InvoiceItemList', 'СведТов', 'ОМ', 'Сведения об отгруженных товарах (о выполненных работах, оказанных услугах), переданных имущественных правах', -1, -1);
  RegisterProperty('TotalForPay', 'ВсегоОпл', 'О', 'Реквизиты строки "Всего к оплате"', -1, -1);
end;

procedure TInvoiceItems.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FInvoiceItemList:=TInvoiceItemList.Create;
  FTotalForPay:=TTotalForPay.Create;
end;

destructor TInvoiceItems.Destroy;
begin
  FreeAndNil(FInvoiceItemList);
  FreeAndNil(FTotalForPay);
  inherited Destroy;
end;

end.

