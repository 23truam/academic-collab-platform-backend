package com.example.academic_collab_platform_backend.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.messaging.simp.config.MessageBrokerRegistry;
import org.springframework.web.socket.config.annotation.EnableWebSocketMessageBroker;
import org.springframework.web.socket.config.annotation.StompEndpointRegistry;
import org.springframework.web.socket.config.annotation.WebSocketMessageBrokerConfigurer;
import org.springframework.web.socket.server.support.DefaultHandshakeHandler;
import org.springframework.web.socket.server.HandshakeInterceptor;
import org.springframework.web.socket.WebSocketHandler;
import org.springframework.web.socket.server.HandshakeHandler;
import org.springframework.http.server.ServerHttpRequest;
import java.security.Principal;
import java.util.Map;
import org.springframework.context.annotation.Bean;
import com.example.academic_collab_platform_backend.config.UserIdHandshakeInterceptor;
import com.example.academic_collab_platform_backend.util.JwtUtil;
import org.springframework.beans.factory.annotation.Autowired;

@Configuration
@EnableWebSocketMessageBroker
public class WebSocketConfig implements WebSocketMessageBrokerConfigurer {

    @Autowired
    private JwtUtil jwtUtil;

    @Bean
    public DefaultHandshakeHandler handshakeHandler() {
        return new DefaultHandshakeHandler() {
            @Override
            protected Principal determineUser(ServerHttpRequest request, WebSocketHandler wsHandler, Map<String, Object> attributes) {
                String userId = (String) attributes.get("userId");
                return () -> userId;
            }
        };
    }

    @Override
    public void registerStompEndpoints(StompEndpointRegistry registry) {
        registry.addEndpoint("/ws")
                .addInterceptors(new UserIdHandshakeInterceptor(jwtUtil))
                .setHandshakeHandler(handshakeHandler())
                .setAllowedOriginPatterns("*")
                .withSockJS();
    }

    @Override
    public void configureMessageBroker(MessageBrokerRegistry registry) {
        registry.setApplicationDestinationPrefixes("/app");
        registry.enableSimpleBroker("/topic", "/queue");
        registry.setUserDestinationPrefix("/user");
    }
} 