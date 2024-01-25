The most up to date code is in the _sale_ branch
a parallel, thread-safe flight reservation system in the programming language Clojure. The system managed flights, their pricing, and customer reservations. 
The goal was to handle multiple booking requests concurrently while ensuring correctness and performance. This entailed implementing the system using various concurrency mechanisms like atoms, refs, etc.,
to enable parallel processing. Then I evaluated the system. Tests were designed to prevent race conditions and corrupt states. Performance evaluation involved varying parameters (e.g., number of flights, customers, sales period duration) 
to measure throughput. In this project I learned a lot about how to guarantee correctness in a parallel application.
