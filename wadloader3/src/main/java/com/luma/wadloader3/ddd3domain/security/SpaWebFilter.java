package com.luma.wadloader3.ddd3domain.security;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.lang.NonNull;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

public class SpaWebFilter extends OncePerRequestFilter {

    @Override
    protected void doFilterInternal(HttpServletRequest request, @NonNull HttpServletResponse response,
                                    @NonNull FilterChain filterChain) throws ServletException, IOException {
        String path = request.getRequestURI();
        Authentication user = SecurityContextHolder.getContext().getAuthentication();

        if (user != null
                && !backendPath(path)
                && !path.contains(".")
                && path.matches("/(.*)")) {
            request.getRequestDispatcher("/").forward(request, response);
            return;
        }
        filterChain.doFilter(request, response);
    }

    private boolean backendPath(String path) {
        return path.startsWith("/wad") || path.startsWith("/wadpack") || path.startsWith("/api") || path.startsWith("/download");
    }
}