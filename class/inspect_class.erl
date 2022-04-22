%%%-------------------------------------------------------------------
%%% @author grzes
%%% @doc
%%% Module for parsing and inspecting JVM Class files.
%%% @end
%%%-------------------------------------------------------------------
-module(inspect_class).

%% API
-export([main/1]).

main([File]) ->
  io:format("Parsing ~s.~n", [File]),
  {ok, Bin} = file:read_file(File),
  inspectClassFile(Bin).

inspectClassFile(Bin) ->
  <<16#cafebabe:32,MinorVersion:16,MajorVersion:16,ConstantPoolCount:16,BinConstantPool/binary>> = Bin,
  io:format("MinorVersion.MajorVersion: ~w.~w.~n", [MinorVersion, MajorVersion]),
  io:format("ConstantPoolCount: ~w.~n", [ConstantPoolCount]),
  {BinClassFlags, CpDict} = inspectConstantPool(BinConstantPool, 1, ConstantPoolCount, dict:new()),
  <<ClassFlags:16, BinThisClass/binary>> = BinClassFlags,
  inspectClassFlags(ClassFlags),
  <<ThisClass:16,SuperClass:16,InterfacesCount:16,BinInterfaces/binary>> = BinThisClass,
  io:format("ThisClass: ~w [~s].~n", [ThisClass, decodeCpIndex(ThisClass, CpDict)]),
  io:format("SuperClass: ~w [~s].~n", [SuperClass, decodeCpIndex(SuperClass, CpDict)]),
  io:format("InterfacesCount: ~w.~n", [InterfacesCount]),
  BinFieldsCount = inspectInterfaces(BinInterfaces, 0, InterfacesCount, CpDict),
  <<FieldsCount:16,BinFields/binary>> = BinFieldsCount,
  io:format("FieldsCount: ~w.~n", [FieldsCount]),
  BinMethodsCount = inspectFields(BinFields, 0, FieldsCount, CpDict),
  <<MethodsCount:16,BinMethods/binary>> = BinMethodsCount,
  io:format("MethodsCount: ~w.~n", [MethodsCount]),
  BinAttributesCount = inspectMethods(BinMethods, 0, MethodsCount, CpDict),
  <<AttributesCount:16,Bin10/binary>> = BinAttributesCount,
  io:format("AttributesCount: ~w.~n", [AttributesCount]),
  Bin11 = inspectAttributes(Bin10, 0, AttributesCount, CpDict),
  io:format("Remaining Bin (expected: <<>>): ~w.~n", [Bin11]).

inspectConstantPool(Bin, K, ConstantPoolCount, CpDict) when K =:= ConstantPoolCount -> {Bin, CpDict};
inspectConstantPool(<<1, Length:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  {Text, Bin2} = split_binary(Bin, Length),
  Str = binary_to_list(Text),
  io:format("~4.w: Utf8                \"~s\" (~w).~n", [K, Str, Length]),
  inspectConstantPool(Bin2, K+1, ConstantPoolCount, dict:store(K, {utf8, Str}, CpDict));
inspectConstantPool(<<3, Value:32, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: Integer             ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {integer, Value}, CpDict));
inspectConstantPool(<<4, Value:32, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: Float               ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {float, Value}, CpDict));
inspectConstantPool(<<5, Value:64, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: Long                ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+2, ConstantPoolCount, dict:store(K, {long, Value}, CpDict));
inspectConstantPool(<<6, Value:64, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: Double              ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+2, ConstantPoolCount, dict:store(K, {double, Value}, CpDict));
inspectConstantPool(<<7, NameIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: Class               NameIndex: ~w.~n", [K, NameIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {class, NameIndex}, CpDict));
inspectConstantPool(<<8, StringIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: String              StringIndex: ~w.~n", [K, StringIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {string, StringIndex}, CpDict));
inspectConstantPool(<<9, ClassIndex:16, NameAndTypeIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: Fieldref            ClassIndex: ~w, NameAndTypeIndex: ~w.~n", [K, ClassIndex, NameAndTypeIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {fieldref, ClassIndex, NameAndTypeIndex}, CpDict));
inspectConstantPool(<<10, ClassIndex:16, NameAndTypeIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: Methodref           ClassIndex: ~w, NameAndTypeIndex: ~w.~n", [K, ClassIndex, NameAndTypeIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {methodref, ClassIndex, NameAndTypeIndex}, CpDict));
inspectConstantPool(<<11, ClassIndex:16, NameAndTypeIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: InterfaceMethodref  ClassIndex: ~w, NameAndTypeIndex: ~w.~n", [K, ClassIndex, NameAndTypeIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {interfacemethodref, ClassIndex, NameAndTypeIndex}, CpDict));
inspectConstantPool(<<12, NameIndex:16, DescriptorIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: NameAndType         NameIndex: ~w, DescriptorIndex: ~w.~n", [K, NameIndex, DescriptorIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {nameandtype, NameIndex, DescriptorIndex}, CpDict));
inspectConstantPool(<<15, ReferenceKind:8, ReferenceIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: MethodHandle        ReferenceKind: ~w, ReferenceIndex: ~w.~n", [K, ReferenceKind, ReferenceIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {methodhandle, ReferenceKind, ReferenceIndex}, CpDict));
inspectConstantPool(<<16, DescriptorIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: MethodType          DescriptorIndex: ~w.~n", [K, DescriptorIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {methodtype, DescriptorIndex}, CpDict));
inspectConstantPool(<<18, BootstrapMethodAttrIndex:16, NameAndTypeIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: InvokeDynamic       BootstrapMethodAttrIndex: ~w, NameAndTypeIndex: ~w.~n", [K, BootstrapMethodAttrIndex, NameAndTypeIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {invokedynamic, BootstrapMethodAttrIndex, NameAndTypeIndex}, CpDict));
inspectConstantPool(<<Tag, _/binary>>, K, _, _) ->
  io:format("~4.w: unrecognized constant pool tag: ~w.~n", [K, Tag]).

inspectFlag(Flags, FlagBit, Name) ->
  if
    Flags band FlagBit > 0 -> io:format(" 0x~4.16.0B ~s", [FlagBit, Name]);
    true -> io:format("")
  end.

inspectClassFlags(Flags) ->
  <<Flags4:4, Flags3:4, Flags2:4, Flags1:4>> = <<Flags:16>>,
  io:format("AccessFlags: 0x~4.16.0B == ~4.2.0B ~4.2.0B ~4.2.0B ~4.2.0B:", [Flags, Flags4, Flags3, Flags2, Flags1]),
  inspectFlag(Flags, 16#0001, "ACC_PUBLIC"),
  inspectFlag(Flags, 16#0010, "ACC_FINAL"),
  inspectFlag(Flags, 16#0020, "ACC_SUPER"),
  inspectFlag(Flags, 16#0200, "ACC_INTERFACE"),
  inspectFlag(Flags, 16#0400, "ACC_ABSTRACT"),
  inspectFlag(Flags, 16#1000, "ACC_SYNTHETIC"),
  inspectFlag(Flags, 16#2000, "ACC_ANNOTATION"),
  inspectFlag(Flags, 16#4000, "ACC_ENUM"),
  io:format("~n").

inspectInnerClassFlags(K, Flags) ->
  <<Flags4:4, Flags3:4, Flags2:4, Flags1:4>> = <<Flags:16>>,
  io:format("  ~4.w: InnerClassAccessFlags: 0x~4.16.0B == ~4.2.0B ~4.2.0B ~4.2.0B ~4.2.0B:", [K, Flags, Flags4, Flags3, Flags2, Flags1]),
  inspectFlag(Flags, 16#0001, "ACC_PUBLIC"),
  inspectFlag(Flags, 16#0002, "ACC_PRIVATE"),
  inspectFlag(Flags, 16#0004, "ACC_PROTECTED"),
  inspectFlag(Flags, 16#0008, "ACC_STATIC"),
  inspectFlag(Flags, 16#0010, "ACC_FINAL"),
  inspectFlag(Flags, 16#0200, "ACC_INTERFACE"),
  inspectFlag(Flags, 16#0400, "ACC_ABSTRACT"),
  inspectFlag(Flags, 16#1000, "ACC_SYNTHETIC"),
  inspectFlag(Flags, 16#2000, "ACC_ANNOTATION"),
  inspectFlag(Flags, 16#4000, "ACC_ENUM"),
  io:format("~n").


inspectInterfaces(Bin, K, InterfacesCount, _) when K =:= InterfacesCount ->
  Bin;
inspectInterfaces(<<Index:16, BinTail/binary>>, K, InterfacesCount, CpDict) ->
  io:format("~4.w: Index: ~w [~s].~n", [K, Index, decodeCpIndex(Index, CpDict)]),
  inspectInterfaces(BinTail, K+1, InterfacesCount, CpDict).

inspectFieldFlags(Flags) ->
  <<Flags4:4, Flags3:4, Flags2:4, Flags1:4>> = <<Flags:16>>,
  io:format("  AccessFlags: 0x~4.16.0B == ~4.2.0B ~4.2.0B ~4.2.0B ~4.2.0B:", [Flags, Flags4, Flags3, Flags2, Flags1]),
  inspectFlag(Flags, 16#0001, "ACC_PUBLIC"),
  inspectFlag(Flags, 16#0002, "ACC_PRIVATE"),
  inspectFlag(Flags, 16#0004, "ACC_PROTECTED"),
  inspectFlag(Flags, 16#0008, "ACC_STATIC"),
  inspectFlag(Flags, 16#0010, "ACC_FINAL"),
  inspectFlag(Flags, 16#0040, "ACC_VOLATILE"),
  inspectFlag(Flags, 16#0080, "ACC_TRANSIENT"),
  inspectFlag(Flags, 16#1000, "ACC_SYNTHETIC"),
  inspectFlag(Flags, 16#4000, "ACC_ENUM"),
  io:format("~n").

inspectFields(Bin, K, FieldsCount, _) when K =:= FieldsCount ->
  Bin;
inspectFields(<<AccessFlags:16, NameIndex:16, DescriptorIndex:16, AttributesCount:16, BinTail/binary>>, K, FieldsCount, CpDict) ->
  io:format("  Field ~w of ~w:~n", [K+1, FieldsCount]),
  inspectFieldFlags(AccessFlags),
  io:format("  NameIndex: ~w [~s]~n", [NameIndex, decodeCpIndex(NameIndex, CpDict)]),
  io:format("  DescriptorIndex: ~w [~s]~n", [DescriptorIndex, decodeCpIndex(DescriptorIndex, CpDict)]),
  io:format("  AttributesCount: ~w~n", [AttributesCount]),
  Bin = inspectAttributes(BinTail, 0, AttributesCount, CpDict),
  inspectFields(Bin, K+1, FieldsCount, CpDict).


inspectMethodFlags(Flags) ->
  <<Flags4:4, Flags3:4, Flags2:4, Flags1:4>> = <<Flags:16>>,
  io:format("  AccessFlags: 0x~4.16.0B == ~4.2.0B ~4.2.0B ~4.2.0B ~4.2.0B:", [Flags, Flags4, Flags3, Flags2, Flags1]),
  inspectFlag(Flags, 16#0001, "ACC_PUBLIC"),
  inspectFlag(Flags, 16#0002, "ACC_PRIVATE"),
  inspectFlag(Flags, 16#0003, "ACC_PROTECTED"),
  inspectFlag(Flags, 16#0008, "ACC_STATIC"),
  inspectFlag(Flags, 16#0010, "ACC_FINAL"),
  inspectFlag(Flags, 16#0020, "ACC_SYNCHRONIZED"),
  inspectFlag(Flags, 16#0040, "ACC_BRIDGE"),
  inspectFlag(Flags, 16#0080, "ACC_VARARGS"),
  inspectFlag(Flags, 16#0100, "ACC_NATIVE"),
  inspectFlag(Flags, 16#0400, "ACC_ABSTRACT"),
  inspectFlag(Flags, 16#0800, "ACC_STRICT"),
  inspectFlag(Flags, 16#1000, "ACC_SYNTHETIC"),
  io:format("~n").

inspectMethods(Bin, K, MethodsCount, _) when K =:= MethodsCount ->
  Bin;
inspectMethods(<<AccessFlags:16, NameIndex:16, DescriptorIndex:16, AttributesCount:16, BinTail/binary>>, K, MethodsCount, CpDict) ->
  io:format("  Method ~w of ~w:~n", [K+1, MethodsCount]),
  inspectMethodFlags(AccessFlags),
  io:format("  NameIndex: ~w [~s] DescriptorIndex: ~w [~s] AttributesCount: ~w~n",
    [NameIndex, decodeCpIndex(NameIndex, CpDict), DescriptorIndex, decodeCpIndex(DescriptorIndex, CpDict), AttributesCount]),
  Bin = inspectAttributes(BinTail, 0, AttributesCount, CpDict),
  inspectMethods(Bin, K+1, MethodsCount, CpDict).

inspectAttributes(Bin, K, AttributesCount, _) when K =:= AttributesCount ->
  Bin;
inspectAttributes(<<AttributeNameIndex:16, AttributeLength:32, BinTail/binary>>, K, AttributesCount, CpDict) ->
  io:format("  Attribute ~w/~w: AttributeNameIndex: ~w [~s] AttributeLength: ~w~n",
    [K+1, AttributesCount, AttributeNameIndex, decodeCpIndex(AttributeNameIndex, CpDict), AttributeLength]),
  {ok, {utf8, AttributeName}} = dict:find(AttributeNameIndex, CpDict),
  {AttributeBin, BinTail2} = split_binary(BinTail, AttributeLength),
  inspectAttribute(AttributeName, AttributeBin, CpDict),
  inspectAttributes(BinTail2, K+1, AttributesCount, CpDict).

inspectAttribute("Code", <<MaxStack:16,MaxLocals:16,CodeLength:32,Bin/binary>>, CpDict) ->
  io:format("  MaxStack: ~w MaxLocals: ~w CodeLength: ~w~n", [MaxStack, MaxLocals, CodeLength]),
  {CodeBin, BinTail1} = split_binary(Bin, CodeLength),
  inspectCode(CodeBin, 0, CodeLength, CpDict),
  <<ExceptionTableLength:16, BinTail2/binary>> = BinTail1,
  io:format("  ExceptionTableLength: ~w~n", [ExceptionTableLength]),
  {ExceptionTableBin, BinTail3} = split_binary(BinTail2, 8*ExceptionTableLength),
  inspectExceptionTable(ExceptionTableBin, 0, ExceptionTableLength, CpDict),
  <<AttributesCount:16, BinTail4/binary>> = BinTail3,
  io:format("  AttributesCount: ~w~n", [AttributesCount]),
  inspectAttributes(BinTail4, 0, AttributesCount, CpDict);
inspectAttribute("InnerClasses", <<NumberOfClasses:16,Bin/binary>>, CpDict) ->
  io:format("  NumberOfClasses: ~w~n", [NumberOfClasses]),
  {ClassesBin, <<>>} = split_binary(Bin, NumberOfClasses*8),
  inspectInnerClass(ClassesBin, 0, NumberOfClasses, CpDict);
inspectAttribute("Signature", <<SignatureIndex:16>>, CpDict) ->
  io:format("  SignatureIndex: ~w [~s]~n", [SignatureIndex, decodeCpIndex(SignatureIndex, CpDict)]);
inspectAttribute("SourceFile", <<SourceFileIndex:16>>, CpDict) ->
  io:format("  SourceFileIndex: ~w [~s]~n", [SourceFileIndex, decodeCpIndex(SourceFileIndex, CpDict)]);
inspectAttribute(Attr, <<AttrBin/binary>>, _) ->
  io:format("  Attribute ~s binary content: ~w~n", [Attr, AttrBin]).

inspectExceptionTable(_, K, ExceptionTableLength, _) when K =:= ExceptionTableLength ->
  io:format("");
inspectExceptionTable(<<StartPc:16, EndPc:16, HandlerPc:16, CatchType:16, BinTail/binary>>, K, ExceptionTableLength, CpDict) ->
  io:format("  ~4.w: StartPc: ~w EndPc: ~w HandlerPc: ~w CatchType: ~s~n", [K, StartPc, EndPc, HandlerPc, decodeCpIndex(CatchType, CpDict)]),
  inspectExceptionTable(BinTail, K+1, ExceptionTableLength, CpDict).

inspectInnerClass(_, K, NumberOfClasses, _) when K =:= NumberOfClasses ->
  io:format("");
inspectInnerClass(<<InnerClassInfoIndex:16, OuterClassInfoIndex:16, InnerNameIndex:16, InnerClassAccessFlags:16, BinTail/binary>>, K, NumberOfClasses, CpDict) ->
  io:format("  ~4.w: InnerClassInfoIndex: ~w [~s]~n", [K, InnerClassInfoIndex, decodeCpIndex(InnerClassInfoIndex, CpDict)]),
  io:format("  ~4.w: OuterClassInfoIndex: ~w [~s]~n", [K, OuterClassInfoIndex, decodeCpIndex(OuterClassInfoIndex, CpDict)]),
  io:format("  ~4.w: InnerNameIndex: ~w [~s]~n", [K, InnerNameIndex, decodeCpIndex(InnerNameIndex, CpDict)]),
  inspectInnerClassFlags(K, InnerClassAccessFlags),
  inspectInnerClass(BinTail, K+1, NumberOfClasses, CpDict).

inspectCode(_, K, CodeLength, _) when K =:= CodeLength -> io:format("");
inspectCode(<<2, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("iconst_m1",  BinTail, K, CodeLength, CpDict);
inspectCode(<<3, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("iconst_0",  BinTail, K, CodeLength, CpDict);
inspectCode(<<4, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("iconst_1",  BinTail, K, CodeLength, CpDict);
inspectCode(<<5, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("iconst_2",  BinTail, K, CodeLength, CpDict);
inspectCode(<<6, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("iconst_3",  BinTail, K, CodeLength, CpDict);
inspectCode(<<7, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("iconst_4",  BinTail, K, CodeLength, CpDict);
inspectCode(<<8, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("iconst_5",  BinTail, K, CodeLength, CpDict);
inspectCode(<<16, Index:8, BinTail/binary>>, K, CodeLength, CpDict) -> opcode2index("bipush", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<18, Index:8, BinTail/binary>>, K, CodeLength, CpDict) -> opcode2index("ldc", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<20, Index:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3index("ldc2_w", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<21, Index:8, BinTail/binary>>, K, CodeLength, CpDict) -> opcode2number("iload", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<25, Index:8, BinTail/binary>>, K, CodeLength, CpDict) -> opcode2number("aload", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<26, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("iload_0",  BinTail, K, CodeLength, CpDict);
inspectCode(<<27, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("iload_1",  BinTail, K, CodeLength, CpDict);
inspectCode(<<28, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("iload_2",  BinTail, K, CodeLength, CpDict);
inspectCode(<<29, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("iload_3",  BinTail, K, CodeLength, CpDict);
inspectCode(<<30, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("lload_0",  BinTail, K, CodeLength, CpDict);
inspectCode(<<31, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("lload_1",  BinTail, K, CodeLength, CpDict);
inspectCode(<<32, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("lload_2",  BinTail, K, CodeLength, CpDict);
inspectCode(<<33, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("lload_3",  BinTail, K, CodeLength, CpDict);
inspectCode(<<42, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("aload_0",  BinTail, K, CodeLength, CpDict);
inspectCode(<<43, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("aload_1",  BinTail, K, CodeLength, CpDict);
inspectCode(<<44, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("aload_2",  BinTail, K, CodeLength, CpDict);
inspectCode(<<45, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("aload_3",  BinTail, K, CodeLength, CpDict);
inspectCode(<<46, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("iaload",  BinTail, K, CodeLength, CpDict);
inspectCode(<<50, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("aaload",  BinTail, K, CodeLength, CpDict);
inspectCode(<<54, Index:8, BinTail/binary>>, K, CodeLength, CpDict) -> opcode2number("istore", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<58, Index:8, BinTail/binary>>, K, CodeLength, CpDict) -> opcode2number("astore", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<59, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("istore_0",  BinTail, K, CodeLength, CpDict);
inspectCode(<<60, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("istore_1",  BinTail, K, CodeLength, CpDict);
inspectCode(<<61, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("istore_2",  BinTail, K, CodeLength, CpDict);
inspectCode(<<62, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("istore_3",  BinTail, K, CodeLength, CpDict);
inspectCode(<<63, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("lstore_0",  BinTail, K, CodeLength, CpDict);
inspectCode(<<64, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("lstore_1",  BinTail, K, CodeLength, CpDict);
inspectCode(<<65, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("lstore_2",  BinTail, K, CodeLength, CpDict);
inspectCode(<<66, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("lstore_3",  BinTail, K, CodeLength, CpDict);
inspectCode(<<75, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("astore_0",  BinTail, K, CodeLength, CpDict);
inspectCode(<<76, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("astore_1",  BinTail, K, CodeLength, CpDict);
inspectCode(<<77, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("astore_2",  BinTail, K, CodeLength, CpDict);
inspectCode(<<78, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("astore_3",  BinTail, K, CodeLength, CpDict);
inspectCode(<<79, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("iastore",  BinTail, K, CodeLength, CpDict);
inspectCode(<<89, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("dup",  BinTail, K, CodeLength, CpDict);
inspectCode(<<96, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("iadd",  BinTail, K, CodeLength, CpDict);
inspectCode(<<97, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("ladd",  BinTail, K, CodeLength, CpDict);
inspectCode(<<100, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("isub",  BinTail, K, CodeLength, CpDict);
inspectCode(<<104, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("imul",  BinTail, K, CodeLength, CpDict);
inspectCode(<<132, Index:8, Const:8, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3numbernumber("iinc", Index, signed8(Const), BinTail, K, CodeLength, CpDict);
inspectCode(<<133, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("il2",  BinTail, K, CodeLength, CpDict);
inspectCode(<<153, Offset:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3number("ifeq", Offset, BinTail, K, CodeLength, CpDict);
inspectCode(<<154, Offset:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3number("ifne", Offset, BinTail, K, CodeLength, CpDict);
inspectCode(<<155, Offset:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3number("iflt", Offset, BinTail, K, CodeLength, CpDict);
inspectCode(<<156, Offset:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3number("ifge", Offset, BinTail, K, CodeLength, CpDict);
inspectCode(<<157, Offset:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3number("ifgt", Offset, BinTail, K, CodeLength, CpDict);
inspectCode(<<158, Offset:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3number("ifle", Offset, BinTail, K, CodeLength, CpDict);
inspectCode(<<159, Offset:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3number("if_icmpeq", Offset, BinTail, K, CodeLength, CpDict);
inspectCode(<<160, Offset:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3number("if_icmpne", Offset, BinTail, K, CodeLength, CpDict);
inspectCode(<<161, Offset:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3number("if_icmplt", Offset, BinTail, K, CodeLength, CpDict);
inspectCode(<<162, Offset:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3number("if_icmpge", Offset, BinTail, K, CodeLength, CpDict);
inspectCode(<<163, Offset:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3number("if_icmpgt", Offset, BinTail, K, CodeLength, CpDict);
inspectCode(<<164, Offset:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3number("if_icmple", Offset, BinTail, K, CodeLength, CpDict);
inspectCode(<<167, Number:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3number("goto", signed16(Number), BinTail, K, CodeLength, CpDict);
inspectCode(<<172, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("ireturn",  BinTail, K, CodeLength, CpDict);
inspectCode(<<173, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("lreturn",  BinTail, K, CodeLength, CpDict);
inspectCode(<<176, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("areturn",  BinTail, K, CodeLength, CpDict);
inspectCode(<<177, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("return",  BinTail, K, CodeLength, CpDict);
inspectCode(<<178, Index:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3index("getstatic", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<179, Index:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3index("putstatic", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<180, Index:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3index("getfield", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<181, Index:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3index("putfield", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<182, Index:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3index("invokevirtual", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<183, Index:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3index("invokespecial", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<184, Index:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3index("invokestatic", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<185, Index:16, _Count, 0, BinTail/binary>>, K, CodeLength, CpDict) -> opcode5index("invokeinterface", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<186, Index:16, 0, 0, BinTail/binary>>, K, CodeLength, CpDict) -> opcode5index("invokedynamic", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<187, Index:16, BinTail/binary>>, K, CodeLength, CpDict) -> opcode3index("new", Index, BinTail, K, CodeLength, CpDict);
inspectCode(<<188, AType:8, BinTail/binary>>, K, CodeLength, CpDict) -> opcode2decode("newarray", AType, decodeArrayType(AType), BinTail, K, CodeLength, CpDict);
inspectCode(<<190, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("arraylength",  BinTail, K, CodeLength, CpDict);
inspectCode(<<191, BinTail/binary>>, K, CodeLength, CpDict) -> opcode1("athrow",  BinTail, K, CodeLength, CpDict);
inspectCode(<<OpCode:8, _/binary>>, K, _, _) -> io:format("  ~4.w: Unrecognized opcode: ~w~n", [K, OpCode]).

signed8(N) ->
  <<Sign:1, Value:7>> = <<N:8>>,
  if
    Sign =:= 0 -> Value;
    true -> -(128 - Value)
  end.

signed16(N) ->
  <<Sign:1, Value:15>> = <<N:16>>,
  if
    Sign =:= 0 -> Value;
    true -> -(32768 - Value)
  end.

decodeArrayType(4) -> "T_BOOLEAN";
decodeArrayType(5) -> "T_CHAR";
decodeArrayType(6) -> "T_FLOAT";
decodeArrayType(7) -> "T_DOUBLE";
decodeArrayType(8) -> "T_BYTE";
decodeArrayType(9) -> "T_SHORT";
decodeArrayType(10) -> "T_INT";
decodeArrayType(11) -> "T_LONG".

opcode1(Name, BinTail, K, CodeLength, CpDict) ->
  io:format("  ~4.w: ~s~n", [K, Name]),
  inspectCode(BinTail, K+1, CodeLength, CpDict).
opcode2number(Name, Number, BinTail, K, CodeLength, CpDict) ->
  io:format("  ~4.w: ~s ~w~n", [K, Name, Number]),
  inspectCode(BinTail, K+2, CodeLength, CpDict).
opcode2decode(Name, Number, NumberDecoded, BinTail, K, CodeLength, CpDict) ->
  io:format("  ~4.w: ~s ~w [~s]~n", [K, Name, Number, NumberDecoded]),
  inspectCode(BinTail, K+2, CodeLength, CpDict).
opcode2index(Name, Index, BinTail, K, CodeLength, CpDict) ->
  io:format("  ~4.w: ~s ~w [~s]~n", [K, Name, Index, decodeCpIndex(Index, CpDict)]),
  inspectCode(BinTail, K+2, CodeLength, CpDict).
opcode3number(Name, Number, BinTail, K, CodeLength, CpDict) ->
  io:format("  ~4.w: ~s ~w~n", [K, Name, Number]),
  inspectCode(BinTail, K+3, CodeLength, CpDict).
opcode3numbernumber(Name, Number1, Number2, BinTail, K, CodeLength, CpDict) ->
  io:format("  ~4.w: ~s ~w ~w~n", [K, Name, Number1, Number2]),
  inspectCode(BinTail, K+3, CodeLength, CpDict).
opcode3index(Name, Index, BinTail, K, CodeLength, CpDict) ->
  io:format("  ~4.w: ~s ~w [~s]~n", [K, Name, Index, decodeCpIndex(Index, CpDict)]),
  inspectCode(BinTail, K+3, CodeLength, CpDict).
opcode5index(Name, Index, BinTail, K, CodeLength, CpDict) ->
  io:format("  ~4.w: ~s ~w [~s]~n", [K, Name, Index, decodeCpIndex(Index, CpDict)]),
  inspectCode(BinTail, K+5, CodeLength, CpDict).

decodeCpIndex(0, _CpDict) -> "0";
decodeCpIndex(Index, CpDict) ->
  {ok, Entry} = dict:find(Index, CpDict),
  decodeCpEntry(Entry, CpDict).

decodeCpEntry({utf8, Str}, _) -> Str;
decodeCpEntry({integer, _Value}, _) -> "integer X";
decodeCpEntry({float, _Value}, _) -> "float X";
decodeCpEntry({long, _Value}, _) -> "long X";
decodeCpEntry({double, _Value}, _) -> "double X";
decodeCpEntry({class, NameIndex}, CpDict) -> "class " ++ decodeCpIndex(NameIndex, CpDict);
decodeCpEntry({string, StringIndex}, CpDict) -> "string " ++ decodeCpIndex(StringIndex, CpDict);
decodeCpEntry({fieldref, ClassIndex, NameAndTypeIndex}, CpDict) -> "fieldref " ++ decodeCpIndex(ClassIndex, CpDict) ++ " " ++ decodeCpIndex(NameAndTypeIndex, CpDict);
decodeCpEntry({methodref, ClassIndex, NameAndTypeIndex}, CpDict) -> "methodref " ++ decodeCpIndex(ClassIndex, CpDict) ++ " " ++ decodeCpIndex(NameAndTypeIndex, CpDict);
decodeCpEntry({interfacemethodref, ClassIndex, NameAndTypeIndex}, CpDict) -> "interfacemethodref " ++ decodeCpIndex(ClassIndex, CpDict) ++ " " ++ decodeCpIndex(NameAndTypeIndex, CpDict);
decodeCpEntry({nameandtype, NameIndex, DescriptorIndex}, CpDict) -> "nameandtype " ++ decodeCpIndex(NameIndex, CpDict) ++ " " ++ decodeCpIndex(DescriptorIndex, CpDict);
decodeCpEntry({methodhandle, ReferenceKind, ReferenceIndex}, CpDict) -> "methodhandle " ++ decodeReferenceKind(ReferenceKind) ++ " " ++ decodeCpIndex(ReferenceIndex, CpDict);
decodeCpEntry({methodtype, DescriptorIndex}, CpDict) -> "methodtype " ++ decodeCpIndex(DescriptorIndex, CpDict);
decodeCpEntry({invokedynamic, _BootstrapMethodAttrIndex, NameAndTypeIndex}, CpDict) -> "invokedynamic " ++ "???TODO???" ++ " " ++ decodeCpIndex(NameAndTypeIndex, CpDict).

decodeReferenceKind(1) -> "REF_getField";
decodeReferenceKind(2) -> "REF_getStatic";
decodeReferenceKind(3) -> "REF_putField";
decodeReferenceKind(4) -> "REF_putStatic";
decodeReferenceKind(5) -> "REF_invokeVirtual";
decodeReferenceKind(6) -> "REF_invokeStatic";
decodeReferenceKind(7) -> "REF_invokeSpecial";
decodeReferenceKind(8) -> "REF_newInvokeSpecial";
decodeReferenceKind(9) -> "REF_invokeInterface".
