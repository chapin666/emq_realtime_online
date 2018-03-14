
emq-realtime-online plugin
===================

This is a realtime-online plugin for the EMQ broker.

config & compile
-------------
1. clone emq-relx 项目:
```
git clone https://github.com/emqtt/emq-relx.git
```

2. Makefile 增加 DEPS:
```
DEPS += emq_realtime_online
dep_emq_realtime_online = git https://github.com/chapin666/emq_realtime_online
```

3. relx.config 中 release 段落添加:
```
{emq_realtime_online, load},
```

4. 编译
```
make
```

License
-------

Apache License Version 2.0
