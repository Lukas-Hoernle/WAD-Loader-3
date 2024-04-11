package com.luma.wadloader3.ddd3domain.security;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

public class SpaWebFilter extends OncePerRequestFilter {

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response,
                                    FilterChain filterChain) throws ServletException, IOException {
        String path = request.getRequestURI();
        System.out.println(path);
        Authentication user = SecurityContextHolder.getContext().getAuthentication();

        if (user != null && !path.startsWith("/api") && !path.contains(".") && path.matches("/(.*)")) {
            System.out.print("Redirecting from: ");
            System.out.println(request.getRequestURI());
            request.getRequestDispatcher("/").forward(request, response);
            System.out.print("Redirected to: ");
            System.out.println(request.getRequestURI());
            return;
        }
        filterChain.doFilter(request, response);
    }
}