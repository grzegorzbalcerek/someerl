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
  <<16#cafebabe:32,MinorVersion:16,MajorVersion:16,ConstantPoolCount:16,BinCP/binary>> = Bin,
  io:format("MinorVersion: ~w.~n", [MinorVersion]),
  io:format("MajorVersion: ~w.~n", [MajorVersion]),
  io:format("ConstantPoolCount: ~w.~n", [ConstantPoolCount]),
  BinAfterConstantPool = inspectConstantPool(BinCP, 1, ConstantPoolCount),
  BinAfterAccessFlags = inspectAccessFlags(BinAfterConstantPool),
  <<ThisClass:16,SuperClass:16,InterfacesCount:16,BinInterfaces/binary>> = BinAfterAccessFlags,
  io:format("ThisClass: ~w.~n", [ThisClass]),
  io:format("SuperClass: ~w.~n", [SuperClass]),
  io:format("InterfacesCount: ~w.~n", [InterfacesCount]).

inspectConstantPool(Bin, K, ConstantPoolCount) when K == ConstantPoolCount -> Bin;
inspectConstantPool(<<1, Length:16, Bin/binary>>, K, ConstantPoolCount) ->
  {Text, Bin2} = split_binary(Bin, Length),
  io:format("#~4.w: Utf8                \"~s\" (~w).~n", [K, binary_to_list(Text), Length]),
  inspectConstantPool(Bin2, K+1, ConstantPoolCount);
inspectConstantPool(<<3, Value:32, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("#~4.w: Integer             ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<4, Value:32, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("#~4.w: Float               ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<5, Value:64, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("#~4.w: Long                ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+2, ConstantPoolCount);
inspectConstantPool(<<6, Value:64, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("#~4.w: Double              ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+2, ConstantPoolCount);
inspectConstantPool(<<7, NameIndex:16, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("#~4.w: Class               NameIndex: ~w.~n", [K, NameIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<8, StringIndex:16, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("#~4.w: String              StringIndex: ~w.~n", [K, StringIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<9, ClassIndex:16, NameAndTypeIndex:16, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("#~4.w: Fieldref            ClassIndex: ~w, NameAndTypeIndex: ~w.~n", [K, ClassIndex, NameAndTypeIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<10, ClassIndex:16, NameAndTypeIndex:16, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("#~4.w: Methodref           ClassIndex: ~w, NameAndTypeIndex: ~w.~n", [K, ClassIndex, NameAndTypeIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<11, ClassIndex:16, NameAndTypeIndex:16, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("#~4.w: InterfaceMethodref  ClassIndex: ~w, NameAndTypeIndex: ~w.~n", [K, ClassIndex, NameAndTypeIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<12, NameIndex:16, DescriptorIndex:16, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("#~4.w: NameAndType         NameIndex: ~w, DescriptorIndex: ~w.~n", [K, NameIndex, DescriptorIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<Tag, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("#~4.w: unrecognized constant pool tag: ~w.~n", [K, Tag]).

inspectAccessFlags(BinAfterConstantPool) ->
  <<AccessFlags:2/binary, Bin/binary>> = BinAfterConstantPool,
  <<FlagsInt:16>> = <<AccessFlags:2/binary>>,
  io:format("AccessFlags: 0x~4.16.0B.~n", [FlagsInt]),
  <<_:1, AccEnum:1, AccAnnotation:1, AccSynthetic:1, _:1, AccAbstract:1, AccInterface:1, _:3, AccSuper:1, AccFinal:1, _:3, AccPublic:1>> = AccessFlags,
  io:format("  AccEnum: ~w.~n", [AccEnum]),
  io:format("  AccAnnotation: ~w.~n", [AccAnnotation]),
  io:format("  AccSynthetic: ~w.~n", [AccSynthetic]),
  io:format("  AccAbstract: ~w.~n", [AccAbstract]),
  io:format("  AccInterface: ~w.~n", [AccInterface]),
  io:format("  AccSuper: ~w.~n", [AccSuper]),
  io:format("  AccFinal: ~w.~n", [AccFinal]),
  io:format("  AccPublic: ~w.~n", [AccPublic]),
  Bin.
