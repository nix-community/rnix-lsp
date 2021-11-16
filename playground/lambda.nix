{ hello }:

rec {
    foo.bar = 13;
    xyz = foo;
    bbb = hello;
    abc = xyz.bar;
}.xyz.bar
