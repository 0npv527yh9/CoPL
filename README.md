# Eval1
- [x] `+` と `plus` の区別
    - 区別してない
- [ ] 以前のシステムで，BinOpのderiveは`rule_of_op`を加えることでまとめられる．

# EvalML1Err
- [ ] 実装する？
```
1 + true + 2 evalto error by E-PlusBoolL {  
    1 + true evalto error by E-PlusBoolR {
        true evalto true by E-True {};
    }
}

if 2 + 3 then 1 else 3 evalto error by E-IfInt {
    2 + 3 evalto 5 by E-Plus {
        2 evalto 2 by E-Int {};
        3 evalto 3 by E-Int {};
        2 plus 3 is 5 by B-Plus {}
    }
}

if 3 < 4 then 1 < true else 3 - false evalto error by E-IfTError {
    3 < 4 evalto true by E-Lt {
        3 evalto 3 by E-Int {};
        4 evalto 4 by E-Int {};
        3 less than 4 is true by B-Lt {}
    };
    1 < true evalto error by E-LtBoolR {
        true evalto true by E-Bool{};
    }
}
```

# EvalML2
- [x] 環境の実装: `variable * value list`
- [ ] `|-` の名前: `PROVE`
