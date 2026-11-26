# dds

纯 Racket 的 DDS/RTPS 协议栈：CDR（XCDR1）序列化、RTPS 2.3 线格式、
SPDP 参与者发现、UDP 多播/单播端点。与 DDS 生态线级兼容——Cyclone
DDS、eProsima Fast-DDS、ROS 2——零 FFI：包只依赖 `base`。

配套的 [`someip`](https://github.com/turlamai/someip) 包实现
SOME/IP；两者一起覆盖 Adaptive AUTOSAR 的两大通信绑定，支撑基于
Racket 的汽车工具链（监听、回放、桥接）。

## 状态 / 路线图

| 阶段 | 范围 | 状态 |
|---|---|---|
| P0 | CDR/XCDR1 编解码、封装头、对齐 | 完成 |
| P1 | RTPS 消息、UDP 传输、SPDP 发现 | 完成 |
| P2 | SEDP 端点发现、可靠协议子集（heartbeat/acknack/gap） | 计划 |
| P3 | QoS 子集（reliable、transient-local、keep-last）、编程式 topic 类型 | 计划 |
| P4 | 工具面：域监控、CDR dissector、回放、SOME/IP-DDS 桥 | 计划 |

`HEARTBEAT`/`ACKNACK`/`GAP` 子消息已在线层完成编解码往返，状态机随
P2 落地。

## 示例

```racket
(require dds)

;; 在发现多播组上监听参与者
(define-values (reader handler) (make-spdp-reader #:event! println))

(define ep
  (make-rtps-endpoint
   #:handler (lambda (header subs sender) (handler header subs sender))))

;; 向同一组宣告自己
(define writer
  (make-spdp-writer
   #:send! (lambda (bs) (endpoint-send-multicast! ep "239.255.0.1" 7400 bs))))
```

同网段的 Cyclone DDS / Fast DDS / ROS 2 参与者会出现在 reader 的
记录里（反之他们也看得到我们的宣告）。

## 设计要点

- **对齐**：CDR 对齐从序列化流起点算起、*包含* 4 字节封装头——CDR
  经典陷阱，只在 `(dds cdr)` 里实现一次。
- **端序**：RTPS 每个子消息可翻转端序（flags bit 0），CDR 按封装头；
  本栈默认发小端（生态惯例），收发两侧均支持。
- **SPDP**：Cyclone 风格的启动突发 + 每 5 秒稳态宣告，`STATUS_INFO`
  注销 + lease 到期回收。
- **端口**：标准端口公式（PB=7400、DG=250、PG=2 与 d0..d3），见
  `(dds entities)`。
- **厂商号**：`0x01F0`（OMG 0x01xx 段未分配的尾部）。
- Racket UDP 无 `SO_REUSEADDR`：每端口每进程一个端点；P2 的完整
  participant 布局会拆分 metatraffic/user 单播 socket。

## 开发

```sh
raco test .        # 单元测试，内联在各模块
raco setup dds     # 构建 scribble 文档
```

自动化套件不碰多播；`test/interop.rkt` 是回环冒烟测试，其中也记录了
对真实 Cyclone DDS 的互操作验证步骤。

## 许可

Apache-2.0
